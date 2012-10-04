#If ( 1 )							// 1 to compile; 0 for omit
#pragma rtGlobals = 1				// Use modern global access method.

Menu "Macros"
	"windtrax"
	"windtraxtable"
	"PromptedUnzipArchive"
	"PromptedLoadCSI-file", PromptedLoadCSI()
	"PromptedLoadCSI-dir", PromptedLoadCSI(type=1)
End

// preferences constants
Static StrConstant ksPackageName = "LAR Tools"
Static StrConstant ksPrefsFileName = "LAR_Tools.bin"
Static StrConstant ksPackageSubfolder = "lar_tools"
Static Constant kID_template = 0 						
Static Constant kID_textToIgorTime = 1
Static Constant kID_softSpikeFilter = 2 
Static Constant kID_boxMean = 3
Static Constant kID_nanFill = 4
Static Constant kID_concatDFs = 5
Static Constant kID_wswd = 6
Static Constant kID_loadFiles = 7


	

//-------------------------------------------------------------------------------------------------------------------------------------------------------------
// LoadG2301( [fileList, cols, concat, tsadd] )
//	
// Optional parameters
//	string 	fileList		semicolon separated list of full paths to files to load; 					default: prompt
//	variable	cols			specify which columns to load: 									default: prompt
//							0 	all columns 
//							1	date, time, CO2/CH4/H2O concentrations
//							2	date, time, CO2/CH4 concentrations
//							3 	date, time, CO2/H2O concentrations
//							4	date, time, CH4/H2O concentrations
//							[expand as needed]
//	variable 	concat		1: concat consecutive files; 2: concat & delete sources; 0: do not 		default: prompt
//	variable	tsadd		1: add date+time > timestamp; 2: add & delete sources; 0: do not 		default: prompt
//
// Returns 
// 	variable 	# of files loaded or -1 for failure
//
// If necessary, prompts user to select files via dialog box and confirm optional parameters. Sequentially sorts 
// selected files by name and consecutively loads waves from each file (no prompts) into subfolders named after 
// each file; no subfolders are used if a single file is loaded. Optionally combines date and time waves into new wave 
// and possibly deletes source date/time waves; optionally concatenates waves across subfolders as new waves in 
// current data folder and possibly deletes source subfolders. 
//		case 0: 		// all columns 
//			B += "N='frac_days_since_jan1'; N='frac_hrs_since_jan1'; N='epoch_time'; "
//			B += "N='alarm_status'; N=species; N='solenoid_valves'; N='das_temp'; N='cavity_pressure'; "
//			B += "N='mode_id'; N='co2_conc_sync'; N='ch4_conc_sync'; N='h2o_conc_sync'; "
//			break
//		case 1: 		// CO2/CH4/H2O only
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N=co2_conc_sync; N=ch4_conc_sync; N='h2o_conc_sync'; "
//			break
//		case 2: 		// CO2/CH4 only
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N=co2_conc_sync; N=ch4_conc_sync; N='_skip_'; "
//			break
//		case 3: 		// CO2/H2O only
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N=co2_conc_sync; N='_skip_'; N='h2o_conc_sync'; "
//			break
//		case 4:		// CH4/H2O only
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N=ch4_conc_sync; N='h2o_conc_sync'; "
//			break




// 2011.11.23 	began conversion to generic Picarro loader
// 2011.old 		original developed as LoadG2301
Function LoadPicarro( fileList, modelName, overwrite, concat, tsconv, options, [baseSFname, B] )
	string fileList 					// semicolon separated list of full file paths to load
	string modelName 			// specify model of analyzer:
									// 	"G2301"		CH4/CO2
									// 	"G1103" 	NH3
									// 	....
	variable overwrite 				// nonzero to overwrite existing waves and subfolders
								//	applies to loaded waves and, if created, concatenated waves
	variable concat				// nonzero to concatenate files sequentially after loading; negative to keep sources
	variable tsconv 				// nonzero to convert date/time fields into Igor date-time wave; negative to keep sources
	variable options 				// set bit fields for fine control
		// bit#  value 			-option-
		// 	-	0			put in subfolder = file's name
		// 	0 	1			put in subfolder = file's parent folder name 	(overrides bit#1)
		//	1 	2			put in subfolder = StringFromMaskedVar( <baseSFname>, # in <fileList> )
		//	2 	4	 		skip existing subfolders instead of unique naming when <overwrite>==0
		// 	3 	8 			force individual files to load in subfolders as if <fileList> had 2+ files
	string baseSFname 			// default: "loadPicarro_file\nn"
	string B						// passed-through to LoadWave/B parameter
	
	variable i, j, numFiles = ItemsInList(fileList), numWaves
	string dfname, fullPath, model = RemoveEnding(LowerStr(modelName), ";")
	DFREF tmpDF, savDF = GetDataFolderDFR()
	Make/DF/FREE/N=(numFiles) destDFRs = NAN
	baseSFname = SelectString( ParamIsDefault(baseSFname) || !strlen(baseSFname), baseSFname, "loadPicarro_file\nn" )
	B = SelectString( ParamIsDefault(B) || !strlen(B), B, "F=6, N=datew; F=7, N=timew; " )
	
	for ( i=0; i<numFiles; i+=1 ) 
		fullPath = StringFromList(i, fileList) 	
		If ( TestBit(options,0) )			// bit#0: file's parent folder = subfolder name
			dfname = CleanupName(ParseFilePath(0, fullPath, ":", 1, 1), 0)
		elseif ( TestBit(options,1) )		// bit#1: use StringFromMaskedVar + # in <fileList> = subfolder name
			dfname = StringFromMaskedVar(baseSFname, i, fixNNwidth=MinFieldWidth(numFiles))
		else 						// use file's name = subfolder name
			dfname = CleanupName(ParseFilePath(3, fullPath, ":", 0, 0), 0)
		endif
		If (DataFolderExists(dfname) && !overwrite && TestBit(options,2))
			print "LoadPicarro: encountered existing subfolder <"+dfname+"> - skipping file "+fullPath
			continue 	// skip loading 
		endif
		print "LoadPicarro: loading <"+fullPath+"> into <"+dfname+">"
		
		tmpDF = NewFreeDataFolder()
		SetDataFolder tmpDF
		strswitch (model)
			case "g1104": 			// H2S 
			case "g1106": 			// C2H4
			case "g1107": 			// H2CO
			case "g1114": 			// H2O2
			case "g2103":			// NH3
			case "g2203": 			// CH4, C2H2
			case "g2204": 			// CH4, H2S
			case "g2205": 			// HF
				print "LoadPicarro: unsupported analyzer specified <"+modelName+"> - aborting."
				return -1
				break
			case "g2301": 			// CO2, CH4, H2O in air
				LoadWave/A/B=B/J/K=1/L={0,1,0,0,0}/Q/R={English,2,2,2,2,"Year-Month-DayOfMonth",40}/V={"\t, "," $",0,1}/W fullPath
				break
			case "g2301-m": 			// CO2, CH4, H2O for flight
			case "g2311-f": 			// CO2, CH4, H2O for EC flux
			case "g2302": 			// CO2, CH4, H2O in air
			case "g2401": 			// CO, CO2, CH4, H2O in air
			case "g2401-m": 			// CO, CO2, CH4, H2O in flight
			case "g5105-i": 			// N2O
			default: 
				print "LoadPicarro: unsupported analyzer specified <"+modelName+"> - aborting."
				return -1
				break
		endswitch
		
		numWaves = ItemsInList(S_waveNames)
		If ( !numWaves )
			print "LoadPicarro: no waves were loaded from file <"+fullPath+">"
			continue
		endif
		
		If (tsconv)
			WAVE/Z datew, timew
			If ( !WaveExists(datew) )
				print "LoadPicarro: could not locate date wave after loading file <"+fullPath+"> - skipping timestamp conversion."
			elseif ( !WaveExists(timew) )
				print "LoadPicarro: could not locate time wave after loading file <"+fullPath+"> - skipping timestamp conversion."
			elseif ( DimSize(datew,0) != DimSize(timew,0) )
				print "LoadPicarro: date and time waves loaded from <"+fullPath+"> had different lengths - skipping conversion."
			else
				Make/D/N=(datew) timestamp = datew[p] + timew[p]
				If ( tsconv < 0 )
					KillWaves datew, timew
				endif
			endif
		endif
		
		SetDataFolder savDF
		If (numFiles > 1 || TestBit(options,3))
			If (DataFolderExists(dfname))
				If (overwrite)
					KillDataFolder $dfname
				else
					dfname = UniqueName(dfname, 11, 0)
				endif
			endif
			MoveDataFolder tmpDF, :		// drop free folder into this folder
			RenameDataFolder freeroot, $dfname
			destDFRs[i] = $dfname
		else
			// borrow i since there is no more outer loops (numFiles=1)
			for (i=0; i<CountObjectsDFR(tmpDF, 1); i+=1)
				string wname = GetIndexedObjNameDFR(tmpDF, 1, i)
				If (overwrite)
					Duplicate/O tmpDF:$wname, $wname
				elseif (WaveExists($wname))
					Duplicate tmpDF:$wname, $UniqueName(wname, 1, 0)
				else
					Duplicate tmpDF:$wname, $wname
				endif
			endfor
		endif
	endfor
	
	If ( numFiles > 1 && concat )
		ConcatAcrossDFRs( destDFRs, "", overwrite, (concat<0 ? 0 : 1) )
	endif
	return 0
End











Function ProgressWindowTest(indefinite)
	Variable indefinite
	
	string pname = NewProgressWindow()
	Variable i,imax= indefinite ? 1000 : 100
	for(i=0;i<imax;i+=1)
//		string p2name = NewProgressWindow()
//		Variable j,jmax= indefinite ? 1000 : 100
//		for(j=0;j<=jmax;j+=1)
//			Variable t1= ticks
//			do
//			while( ticks < (t1+1) )
//			if( indefinite )
//				UpdateProgressWindow( pname, -1, 0, "")
//			else
//				UpdateProgressWindow( pname, j, jmax, "")
//			endif
//		endfor
		Variable t0= ticks
		do
		while( ticks < (t0+1) )
		if( indefinite )
			UpdateProgressWindow( pname, -1, 0)
		else
			UpdateProgressWindow( pname, i, imax)
		endif
	endfor
	KillWindow $pname
End













//
Function PromptedUnzipArchive()
	string loadPathName, fileList, fileFilter, fileExt, fileName, prefpath, overwriteStr
	variable sortByPick, sortBy, overwrite, flatten

	overwriteStr = "Prompt for instructions;Overwrite without prompt;Skip existing files;"
	overwriteStr += "Rename extracted file;Rename existing file;"
	
	prefpath = "root:Packages:ART:PromptedUnzipArchive"
	DFREF prefs = NewDataFolderX(prefpath)
	fileFilter = StrVarOrDefault(prefpath+":gFileFilter", "")
	fileExt = StrVarOrDefault(prefpath+":gFileExt", "")
	sortByPick = NumVarOrDefault(prefpath+":gSortByPick", 1) // 1=1st choice
	overwrite = NumVarOrDefault(prefpath+":gOverwrite", 1)
	flatten = NumVarOrDefault(prefpath+":gFlatten", 1)
	
	loadPathName = UniqueName("temp", 12, 0)			// location prompt
	NewPath/Q $loadPathName
	If ( V_flag )
		return -1
	endif
	
	Prompt fileFilter, "Grep file name filter:"				// options prompt
	Prompt fileExt, "Four character file extension filter:"
	Prompt sortByPick, "Sort found files by:", popup, puSortOptionList()
	Prompt overwrite, "Overwrite options", popup, overwriteStr
	Prompt flatten, "Flatten archive output to one directory?", popup, "No;Yes;"
	DoPrompt "Unzip Archive", fileFilter, fileExt, sortByPick, overwrite, flatten
	If ( V_flag )
		return -1
	endif
	string/G prefs:gFileFilter = fileFilter
	string/G prefs:gFileExt = fileExt
	variable/G prefs:gSortByPick = sortByPick
	variable/G prefs:gOverwrite = overwrite
	variable/G prefs:gFlatten = flatten
	sortBy = puSortOptionValue( sortByPick )
	
	fileList = ListFilesIn(loadPathName, fileFilter, fileExt, -1, sortBy )
	variable i
	for (i=0; i<ItemsInList(fileList); i+=1)
		fileName = StringFromList(i, fileList)
		DoAlert 1, "Next up: "+ParseFilePath(0, fileName, ":", 1, 0)+"\r\rWould you like to continue?"
		If ( V_flag - 1 )
			break
		endif
		print UnzipArchive( fileName, "", overwrite-1, flatten-1 )
	endfor
	Killpath $loadPathName
end



//
Function PromptedLoadCSI( [type] )
	variable type
	string fileList, prefpath = "root:Packages:ART:PromptedLoadCSI:"
	variable option
	DFREF prefs = NewDataFolderX(prefpath)
	
	string fileFilter = StrVarOrDefault(prefpath+"gFileFilter", "")			// look up preferences
	string fileExt = StrVarOrDefault(prefpath+"gFileExt", "")
	variable recurse = NumVarOrDefault(prefpath+"gRecurse", 0)
	variable level = NumVarOrDefault(prefpath+"gLevel", 0)
	variable choice = NumVarOrDefault(prefpath+"gChoice", 0)	
	variable fileType = NumVarOrDefault(prefpath+"gFileType", 0)
	variable overwrite = NumVarOrDefault(prefpath+"gOverwrite", 0)
	variable convert = NumVarOrDefault(prefpath+"gConvert", 0)
	variable concat = NumVarOrDefault(prefpath+"gConcat", 0)
	variable subfolders = NumVarOrDefault(prefpath+"gSubfolders", 0)
	string mask = StrVarOrDefault(prefpath+"gMask", "loadCSI_file\nn")
	variable subforone = NumVarOrDefault(prefpath+"gSubForOne", 0)
	
	If ( !type || ParamIsDefault(type) )
		Open/D/R/MULT=1/F=ksFileFilter refnum
		If ( !strlen(S_fileName) )
			return -1
		endif
		fileList = ReplaceString(num2char(13), S_fileName, ";")
	else	
		string pname = ListFilesIn_Panel()
		PauseForUser $pname
		SVAR gS_fileName = S_fileName
		fileList = gS_fileName
		KillStrings/Z gS_fileName
	endif
	
	If ( strlen(fileList) )
		Prompt overwrite, "Overwrite existing waves and subfolders?", popup, "No;Yes;Skip existing subfolders;"
		Prompt convert, "Convert string timestamps to Igor date/time?", popup, "No;Yes, delete source;Yes, keep source"
		Prompt concat, "Concatenate sequential files (NOT ACTIVE)?", popup, "No;Yes, delete sources;Yes, keep sources;"
		Prompt subfolders, "For multiple files, name subfolders...", popup, "using file name;using file's parent folder name;using a mask + consecutive integers"
		Prompt mask, "Subfolder mask, if using"
		Prompt subforone, "Create a subfolder even for a single file?", popup, "No;Yes"
		DoPrompt "Load waves options", overwrite, convert, concat, subfolders, mask, subforone
		If (V_flag)
			return -1
		endif
		variable/G prefs:gOverwrite = overwrite							// save changes
		variable/G prefs:gConvert = convert
		variable/G prefs:gConcat = concat
		variable/G prefs:gSubfolders = subfolders
		string/G prefs:gMask = mask
		variable/G prefs:gSubForOne = subforone
		overwrite -= 1		// sets no/yes back to 0/1 for pass-thru			// decode options
		if (overwrite == 2) 		// if skip existing
			overwrite = 0
			option = SetBit(option, 2)
		endif
		convert -= 1 		// set to 0,1,2
		If (convert == 2)
			convert = -1
		endif
		concat -=1		// set to 0,1,2
		If (concat == 2)
			concat = -1
		endif
		if (subfolders == 2) 		// from parents name
			option = SetBit(option, 0)
		elseif (subfolders == 3) 	// from masked var
			option = SetBit(option, 1)
		endif
		if ( subforone - 1 )	// true for yes
			option = SetBit(option, 3)
		endif
		if (strlen(mask))
			LoadCSI( fileList, fileType, overwrite, convert, concat, option, baseSFname=mask)
		else
			LoadCSI( fileList, fileType, overwrite, convert, concat, option)
		endif
	endif
	KillPath/Z loadcsitmp
End





Function PromptedLatLongDistance()
	variable lat1 						// initial point latitude; N=(+), S=(-)
	variable long1					// initial point longitdue; E=(+), W=(-)
	variable lat2						// 2nd point latitude
	variable long2					// 2nd point longitude
	variable units						// unit selection index
	string unitList = "km;m;mi;ft;"  		// available unit choices
	string out						// results string to print
	
	Prompt lat1, "Point 1 decimal latitude: N=(+) S=(-)" 			// ask for latitude point 1
	Prompt long1, "Point 1 decimal longitude: E=(+) W=(-)" 		// ask for longitude point 1
	Prompt lat2, "Point 2 decimal latitude:" 						// ask for latitude point 2 
	Prompt long2, "Point 2 decimal longitude:" 					// ask for longitude point 2
	units += 1												// adjust to 1-indexed
	Prompt units, "Select output units:", popup, unitList			// ask for units selection
	DoPrompt "Haversine Lat-Long Distance Formula", lat1, long1, lat2, long2, units // prompt user
	AbortOnValue V_flag, -1 									// quit if user cancels
	units -= 1												// adjust back to 0-indexed

	switch (units)
		case 0:
			return LatLongDistance( lat1, long1, lat2, long2 )/1000
		case 1:
			return LatLongDistance( lat1, long1, lat2, long2 )
		case 2: 
			return LatLongDistance( lat1, long1, lat2, long2 )*0.6213712/1000	// 0.6213712 mi / km
		case 3:
			return LatLongDistance( lat1, long1, lat2, long2 )*3.28083			// 3.28083 ft / m
	endswitch
	return -1
End


// doesn't handle gracefully without DFRs
Function KillThenMove( src, dest )
	wave src
	string dest
	If ( WaveExists( $dest ) )
		KillWaves/Z $dest
	endif
	If ( WaveExists( $dest ) )
		print "KillThenMove: could not kill wave <"+dest+"> - source not moved"
		return -1
	else
		MoveWave src, $dest
	endif
	return 0
End














//-------------------------------------------------------------------------------------------------------------------------------------------------------------
// LoadNMEA( [fileList] )
// 	string 	fileList 		semicolon separated list of full paths to files to load
//	
// Returns 
// 	variable 	# of files loaded or -1 for failure
//
Function LoadNMEA( [fileList])
	string fileList 										// semicolon separated list of files to load
	
	variable refnum									// file ref #
	variable i, j										// counters
	variable L										// line number
	string DFname									// constructs new datafolders
	string fullPath, filePath, fileName, fileExt				// current working file info
	string code, msg									// type of NMEA message this row & content
	DFREF savDF = GetDataFolderDFR()				// save current location

	If (ParamIsDefault(fileList)) 							// if files aren't specified
		fileList = PromptForFileList("Select GPS files to load") // prompt user 
		If ( !strlen(fileList) ) 								// if cancels
			return -1 										// quit failure
		endif
	endif
	
	for ( i=0; i<ItemsInList(fileList, num2char(13)); i+=1) 	// for each listed file
		fullPath = StringFromList(i, fileList, num2char(13)) 	// extract path from list
		//filePath = ParseFilePath(1, fullPath, ":", 1, 0)			// grab without file name+ext
		//fileName = ParseFilePath(3, fullPath, ":", 0, 0)		// grab file name alone
		//fileExt = ParseFilePath(4, fullPath, ":", 0, 0)			// grab file extension alone

		DFname = "root:tmpGPS_"+CleanupName(fileName, 0) // path to temp directory
		NewDataFolder/O/S $DFname 		 				// build/switch to temp dir
		DFREF tmpDF = $DFname						// make references to folders
		LoadWave/A/B="C=1,F=-2,N=NMEAmsgs;"/J/D/O/K=2/V={""," $",0,0} fullPath // load from file as 1 text wave
		WAVE/Z/T NMEAmsgs							// refer to imported wave
		if ( V_flag == 0 || !WaveExists(NMEAmsgs) )			// if no waves loaded or gone missing
			// lar_print(0, ">\tNo waves were loaded from "+fullPath) 	// msg
			continue 										// skip rest of this loop
		endif
		j = numpnts(NMEAmsgs)							// how many rows loaded?
		
		// NMEA message information taken from the ASPEN-GPS System Operation Manual Appendix M
		NewDataFolder/O/S ALMmsgs						// build/switch-to subfolder
			// $GPALM,1,1,21,768,00,5d94,08,08b7,fd4f,a10d2e,747f49,57ab38, c0b629, ffaa, 000*43
			//	$GPALM		header
			//	1				total # of ALM messages this cycle
			//	1				message sequence #
			//	21				SV PRN #: 01 to 32
			// 	768				GPS week #
			//	00				SV health status
			//	5d94			eccentricity
			//	08 				almanac reference time
			//	08b7			inclination angle
			//	fd4f				rate of right ascension
			//	a10d2e			root of semimajor axis
			//	747f49			argument of perigee
			//	57ab38			longitude of ascension node
			// 	c0b629			mean anomaly
			//	ffaa				a(f0), clock parameter
			//	000				a(f1), clock parameter
			//	*43				CRC checksum
		Make/N=(j)/O/T col01, col02, col03, col04, col05, col06, col07, col08, col09, col10, col11, col12, col13, col14, col15
		SetDataFolder tmpDF								// return to parent folder
		DFREF ALM = ALMmsgs							// create reference
		
		NewDataFolder/O GGAmsgs
			// $GPGGA,004407.00,3723.477595,N,12202.251222,W,2,07,1.2,19.1,M,-25.7,M,3,0003*55
			//	$GPGGA		header
			//	004407.00		UTC of position fix
			//	3723.477595		latitude, ddmm.mmmm
			//	N				direction of latitude (N/S)
			//	12202.251222 	longitude, dddmm.mmmm
			//	W				direction of longitude (E/W)
			//	2				GPS quality indicator: 0=fix not valid; 1=GPS fix; 2=Differential GPS fix
			//	07				# of SVs in use, 00 to 12
			//	1.2				HDOP
			//	19.1				antenna height, MSL reference (meters)
			//	M				fixed unit text for antenna height (meters)
			//	-25.7			geoidal separation (meters)
			//	M				fixed unit text for geoidal separation (meters)
			//	3				age of differential GPS record, Type 1 or Type 9, null when DGPS not used
			//	0003			differential reference station ID, 0000 to 1023, null when "any reference station ID" is
			//					selected and no corrections have been received
			//	*55				CRC checksum
		Make/N=(j)/O/T timeUTC, latitude, lat_dir, longitude, long_dir, fix_level, SV_numused, HDOP, ant_height, geo_sep
		Make/N=(j)/O/T DGPS_age, diff_ID
		SetDataFolder tmpDF
		DFREF GGA = GGAmsgs
		
		NewDataFolder/O GLLmsgs
			// $GPGLL,
		Make/N=(j)/O/T latitude, lat_dir, longitude, long_dir, timeUTC, fix_valid
		SetDataFolder tmpDF
		DFREF GLL = GLLmsgs
		
		NewDataFolder/O GSAmsgs
		Make/N=(j)/O/T fix_mode, fix_state, PRN_1, PRN_2, PRN_3, PRN_4, PRN_5, PRN_6, PRN_7, PRN_8, PRN_9
		Make/N=(j)/O/T PRN_10, PRN_11, PRN_12, PDOP, HDOP, VDOP
		SetDataFolder tmpDF
		DFREF GSA = GSAmsgs

		NewDataFolder/O GSVmsgs
		Make/N=(j)/O/T msg_tot, msg_num, SV_tot, A_SV_PRN, A_elev, A_azimuth, A_SNR, B_SV_PRN, B_elev, B_azimuth
		Make/N=(j)/O/T B_SNR, C_SV_PRN, C_elev, C_azimuth, C_SNR, D_SV_PRN, D_elev, D_azimuth, D_SNR
		SetDataFolder tmpDF
		DFREF GSV = GSVmsgs

		NewDataFolder/O RMCmsgs
		Make/N=(j)/O/T timeUTC, fix_valid, latitude, lat_dir, longitude, long_dir, SOG_knots, track_made_good, ddmmyy, mag_var, mag_var_dir
		SetDataFolder tmpDF
		DFREF RMC = RMCmsgs
		
		NewDataFolder/O VTGmsgs
		Make/N=(j)/O/T track_made_good, SOG_knots, SOG_kmh
		SetDataFolder tmpDF
		DFREF VTG = VTGmsgs

		NewDataFolder/O ZDAmsgs
		Make/N=(j)/O/T timeUTC, day, month, year, GMT_offset_hr, GMT_offset_min
		SetDataFolder tmpDF
		DFREF ZDA = ZDAmsgs
		
		for ( j=0; j<numpnts(NMEAmsgs); j+=1 ) 			// for each message in file
			msg = NMEAmsgs[0]								// save locally
			if ( cmpstr(msg[0],"$") )							// if msg doesn't start with $
				// lar_print(3, "*\tEncountered non-NMEA message at line "+num2istr(j)) // msg
				continue											// skip this row
			endif
			code = msg[3,5] 									// extract 3-char NMEA code
			strswitch (code)									// based on code
				case "ALM":
					break
				case "GGA":
					break
				case "GLL":
					break
				case "GSA":
					break
				case "GSV":
					break
				case "RMC":
					break
				case "VTG":
					break
				case "ZDA":
					break
			endswitch
		endfor
	endfor
	SetDataFolder savDF		
	//KillDataFolder/Z tmpDF
	return 0											// return success
End



// cleanupMask( inStr )
// 	string 	inStr 	input to be cleaned
// 
// returns cleaned up string
// 
// Same basic idea as CleanupName(...) but preserves asterisks in the string
//
Static Function/S cleanupMask( inStr )
	string inStr 							// input string
	// define chars OK to have in mask using strict naming convention (a-z, A-Z, 0-9, _ * )
	string OK = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789*" 
	variable i 							// counter
	
	inStr = ReplaceString(":", inStr, "")		// remove any colons first
	for (i=0; i<strlen(inStr); i+=1) 			// for each character in input string
		if ( strsearch(OK, inStr[i], 0) == -1 )		// if character is not in OK list
			inStr = ReplaceString( inStr[i], inStr, "_", 1, 1) // replace with underscore
		endif
	endfor
	return inStr 							// pass back cleaned string
End


//-------------------------------------------------------------------------------------------------------------------------------------------------------------
// cleanupPath( inPath )
// 	string 	inPath 	path to clean up
//	variable 	rel		nonzero indicates a relative path
//
// returns cleaned up string
// 
// Places single quotes around any liberal-named datafolders. Removes trailing semicolons from all paths and
// removes leading semicolons unless the path is relative (rel=nonzero) in which case one is added if not present
//
Static Function/S cleanupPath( inPath, [rel] )
	string inPath							// input string
	variable rel							// relative path flag
	variable i
	string outPath = ""

	for ( i=0; i<ItemsInList(inPath, ":"); i+=1 ) // for each folder in the path
		outPath = AddListItem(PossiblyQuoteName(StringFromList(i, inPath, ":")), outPath, ":", Inf) // quote, append back of list
	endfor
	if ( rel && cmpstr(outPath[0], ":") )			// if first character isn't a colon
		//outPath = ReplaceString(":",outPath,"",0,1) 	// erases first instance
		outPath = ":"+outPath						// prefix one
	endif
	return RemoveEnding(outPath, ":") 			// remove trailing colon & return
	//return (RemoveEnding(outPath, ":")+":")		// strip trailing colon, if any, then add one & return string
End




//-------------------------------------------------------------------------------------------------------------------------------------------------------------
// intPrint( interval [, decimals] )
// 	variable 	interval 		time interval to format as string, cannot be negative
// 
// Optional parameters
// 	variable 	decimals 	# of digits to display after decimal point
//	variable 	nospace 	nonzero to omit space between # and units
//
// Returns 
// 	string 	size of interval, optional space, and units; ex: "5 min" "500ms" "45.5 days" "2hr" "
// 			empty string if interval is not positive
// 
// If interval size is less than, 	then interval is expressed in
//		1 second				milliseconds
// 		1 minute 				seconds
// 		1 hour 					minutes
//		1 day 					hours
// 		Inf 						days
//
Static Function/S intPrint( interval, [decimals, nospace] )
	variable interval 							// interval to express
	variable decimals 							// # digits to show past decimal
	variable nospace 							// flag to omit space
	
	string num, units 							// output strings
	
	if ( interval <= 0 ) 							// if not positive
		return "" 									// quit failure
	elseif ( interval < 1 ) 						// if less than 1 second
		interval *= 1000							// convert to milliseconds
		units = "ms"								// set units
	elseif ( interval < 60 ) 						// if less than 1 minute
		// do nothing 								// leave in seconds
		units = "sec"								// set units
	elseif ( interval < 3600 )					// if less than 1 hour
		interval /= 60 							// convert to minutes
		units = "min"								// set units
	elseif ( interval < 86400 )					// if less than 1 day
		interval /= 3600 							// convert to hours
		units = "hr"								// set units
	else 									// any interval 1 day or longer
		interval /= 86400							// convert to days
		units = "days"							// set units
	endif
		
	if (decimals) 								// and # of decimal places is set
		sprintf num, "%.*f", decimals, interval 		// format it specifically
	else 									// otherwise
		num = num2str(interval) 					// let Igor format it
	endif
	if (nospace) 								// if space should be absent
		return num+units 							// hand back combined string
	else 									// otherwise
		return num+" "+units						// combine with space in between
	endif
End





///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// 	windArrowTimeSeries( [windDirW, windSpdW, minArrowLen, maxArrowLen, prefix] )
//	
//	Optional Parameters 
//		windDirW		wave	wind directions in decimal degrees
//		windSpdW		wave	wind speeds in any units
//		minArrowLen		variable	length of smallest arrow (pnts) corresponding to minimum wind speed
//		maxArrowLen	variable 	length of longest arrow (pnts) corresponding to maximum wind speed
//		zeroOption		variable  odd = keep markers, no arrows for zero WS; even = no markers/arrows
//		prefix			string	prefix to append to output waves' names
//
//	This function creates two waves: "WD_arrowPoints" & "WD_arrowInfo" and permits the user to 
//		optionally specify a prefix to include in their names. If no prefix is specified, none is added. 
//		The output waves are of the same dimension as the input waves so if time-averaging is desired,
//		it should be performed prior to use of this function.
//	Wind directions are used to determine arrow orientation; graphically, the arrows represent the
//		mean wind vector (ie., points in same direction as wind flow). Entries must be in degrees but 
//		there is no limitation on range of entries (ie., -90 is same as 270)
//	Wind speeds are used to determine the overall arrow size. Dimensions are normalized such that 
//		the smallest length represents the smallest wind speed in the dataset and longest length 
//		represents maximum wind speed. If  the 'minArrowLen' and 'maxArrowLen' variables are not 
//		provided, they assume default values of 4 and 16, respectively. 
//
//	If no parameters are provided, or if the passed wave references are invalid, the user is prompted 
//		to 1) specify the folder path containing the wind speed/direction waves (where the default value
//		is the current datafolder), then 2) select the wind speed/direction waves via drop-down boxes.
//		An opportunity is also provided at this point to change min/max arrow lengths & the prefix.
//
//	To plot this time series of arrows, display WD_arrowPoints and set the point mode to marker 
//		(ModifyGraph mode=3). Since no convenient dialogs exist for changing markers to arrows, 
//		its recommended to just paste the following text into the command window (Ctrl+J)
//					Modifygraph arrowmarker={WD_arrowInfo, 0,0,0,1}
//		where the wave name "WD_arrowInfo" has the appropriate prefix, if necessary
//
//	- pokeeffe
Function windArrowTimeSeries_tb( [windDirW, windSpdW, timeW, minArrowLen, maxArrowLen, zeroOption, prefix] )
	wave/z windDirW			// optional, user-provided information
	wave/z windSpdW
	wave/z timeW
	variable minArrowLen			
	variable maxArrowLen	
	variable zeroOption
	string prefix
	
	// assign defaults if not provided
	minArrowLen = ParamIsDefault(minArrowLen) ? 4 : minArrowLen
	maxArrowLen = ParamIsDefault(maxArrowLen) ? 16 : maxArrowLen
	If ( ParamIsDefault(prefix) )
		prefix = "new_"
	endif
	if ( ParamIsDefault(zeroOption) )
		zeroOption = 1
	endif
	
	string saveFolder			// for saving spots
	string nowFolder			// where to look for waves in case we need to
	string waveNameList		// all waves in specified datafolder
	string timeWname		// in case a prompt for wave names occurs
	string windDirWname		//
	string windSpdWname	//
	variable i					// counter
	variable metAngle			// for converting met coords --> radial math coords
	variable quadrant			// ...
	variable mathAngle		// ...
	variable maxWS			// for normalizing wind speeds & proportioning arrows
	variable minWS			// ...
	variable spanWS			// ...
	variable xWS
	
	// if passed waves were not good or not even passed
	If ( !WaveExists(windDirW) || !WaveExists(windSpdW) )
		nowFolder = GetDataFolder(1)			// where's here?
		saveFolder = nowFolder				// save spot
		Prompt nowFolder, "Look for waves in which data folder? ('Continue' >> current DF)"
		do									// do...
			DoPrompt "Wind Arrow Time Series", nowFolder
			If ( V_flag == 1 )						// if cancel
				abort								// quit
			endif
			If ( DataFolderExists(nowFolder) )		// if good entry
				break;								// exit loop
			endif
		while (1)								// until loop is exited
		SetDataFolder $nowFolder
		waveNameList = WaveList("*",";","")		// find all waves in that folder
		windDirWname = StringFromList(0, waveNameList)	// fill in default values
		windSpdWname = StringFromList(1, waveNameList)
		timeWname = StringFromList(2,waveNameList)
		
		// create prompt & ask user
		Prompt windDirWname, "Select the wind direction wave:", popup, waveNameList
		Prompt windSpdWname, "Select the wind speed wave:", popup, waveNameList
		Prompt timeWname, "Select the date & time wave:", popup, waveNameList
		Prompt minArrowLen, "Length of minimum wind speed arrows (pnts)"
		Prompt maxArrowLen, "Length of maximum wind speed arrows (pnts)"
		Prompt zeroOption, "Show zero wind speed as", popup, "marker (no arrow);no marker or arrow"
		Prompt prefix, "Add prefix to created waves? (in quotes)"
		DoPrompt "Wind Arrow Time Series",windDirWname,windSpdWname,timeWname,minArrowLen,maxArrowLen,zeroOption,prefix
		If ( V_flag == 1 )		// if cancel
			SetDataFolder $saveFolder
			abort				// quit
		endif
		
		wave windDirW = $windDirWname		// set correct references
		wave windSpdW = $windSpdWname
		wave timeW = $timeWname
		
		// if value is less than 4 (min), then reset to 4; >> will double this later so 2 is OK here but 4 looks better
		minArrowLen = (minArrowLen < 4) ? 4 : minArrowLen	
		// if value is less than/equal to minArrowLen, then reset to 4*minArrowLen
		maxArrowLen = (maxArrowLen<=minArrowLen) ? 4*minArrowLen : maxArrowLen
	endif
	
	maxWS = 0			// initialize low
	minWS = 1000		// initialize high
	for ( i=0; i<numpnts(windSpdW); i+=1 )		// for each point in wind speed wave
		// compare WS[i] to stored max/min values
		maxWS = ( windSpdW[i] > maxWS ) ? windSpdW[i] : maxWS		// reassign if WS[i] is higher
		minWS = ( windSpdW[i] < minWS ) ? windSpdW[i] : minWS		// reassign if WS[i] is lower
	endfor
	spanWS = maxWS - minWS

	// make wave of points to put arrow markers on; add prefix but reference generically
	Make/D/O/N=(numpnts(windDirW)) $(prefix+"WD_arrowPoints") = NaN
	wave WD_arrowPntsByDay = $(prefix+"WD_arrowPoints")	
	// make wave of arrow information; add prefix but reference generically
	Make/D/O/N=(numpnts(windDirW),5) $(prefix+"WD_arrowInfo") = NaN		
	wave WD_arrowInfo = $(prefix+"WD_arrowInfo")
	
	variable/G $(prefix+"WDarrow_maxWS") = maxWS
	variable/G $(prefix+"WDarrow_minWS") = minWS
		
	// WD_arrowInfo[][0] = arrow line lengths
	// WD_arrowInfo[][1] = arrow angles (rad), CCW from horizontal right-ward
	SetDimLabel 1, 2, lineThick, WD_arrowInfo		// WD_arrowInfo[][2] = arrow line thickness
	SetDimLabel 1, 3, headLen, WD_arrowInfo 		// WD_arrowInfo[][3] = arrow head length
	SetDimLabel 1, 4, headFat, WD_arrowInfo 		// WD_arrowInfo[][4] = arrow head width ratio
	
	for ( i=0; i<numpnts(windDirW); i+=1 )
		if ( numtype(windDirW[i]) != 0 )		// if not a normal number
			mathAngle = NaN
		else
			metAngle = modWD(windDirW[i])		// ensure wind dir range: (0, 360]
			quadrant = trunc(metAngle/90) + 1
				// for 0 <= metAngle < 90		quadrant = 0+1 = 1
				// for 90 <= metAngle < 180	quadrant = 1+1 = 2
				// for 180 <= metAngle < 270	quadrant = 2+1 = 3
				// for 270 <= metAngle < 360	quadrant = 3+1 = 4
			
			//			 metAngle							mathAngle 
			//				0									90
			//				|									|
			//			4	|	1								|
			//				|									|
			//	270 -------------------------- 90				180 ---------------------------- 0
			//				|									|
			//			3	|	2								|
			//				|									|
			//				180									270
			
			switch ( quadrant )
				case 1:				 
					mathAngle = 90 - metAngle
					break;
				case 2:
					mathAngle = 270 + (180-metAngle)
					break;
				case 3:
					mathAngle = 180 + (270-metAngle)
					break;
				case 4:
					mathAngle = 180 - (metAngle-270)
					break;		
			endswitch
			
			// metAngle is angle of the direction wind is approaching
			// mathAngle is angle of the direction wind is heading
			//		so add 180 first
			// to graph, angles should not be negative & should be radians
			//		so ensure range is (0, 360], then convert to rad.
			mathAngle = modWD(mathAngle+180) * (PI/180)
		endif
		
		// Arrow proportions:				where:	
		//	arrow length: 2X					X=4 @ min. WS
		// 	arrow angle: --					X=15 @ max. WS
		//	line thickness: 0.3X				X(WS[i]) = minLen+(spanLen)*(WS[i]-minWS)/spanWS
		//	head length: X					spanWS = (maxWS - minWS)
		// 	head width ratio: 0.9
		xWS = minArrowLen + (maxArrowLen-minArrowLen)*(windSpdW[i] - minWS)/spanWS
		
		// define arrow properties
		WD_arrowInfo[i][0] = 2*xWS						// arrow length
		WD_arrowInfo[i][1] = mathAngle					// arrow angle
		WD_arrowInfo[i][%lineThick] = 0.3*xWS				// line thickness
		WD_arrowInfo[i][%headLen] = xWS					// head length
		WD_arrowInfo[i][%headFat] = 0.9					// head width
		
		if ( minWS<=0 && xWS==minArrowLen )			// in case min WS is zero & this record has a min WS
			switch ( zeroOption )								// check zero option selection
				case 2:											// if 'don't display arrow'
					WD_arrowPntsByDay[i] = NaN						// don't put an arrow b/c avg WS == 0
					break;
				default:											// by default
					WD_arrowInfo[i][0] = 1								// set arrow length <4 so default marker shows up
					break;
			endswitch
		endif
		WD_arrowPntsByDay[i] = trunc(timeW[i]/86400) * 86400	// save just date to points wave
	endfor
	
	Duplicate/D/O WD_arrowPntsByDay, $(prefix+"WD_arrowPoints")
	wave WD_arrowPoints = $(prefix+"WD_arrowPoints")
	WD_arrowPoints = 1
	
	DoWindow/K pasteFrom
	NewNotebook/F=0/K=1/W=(378,188.75,638.25,282.5)/N=pasteFrom 
	Notebook pasteFrom text="Modifygraph arrowmarker={"+NameOfWave(WD_arrowInfo)+", 0,0,0,1}"
end	// windArrowTimeSeries

Function showArrowMarkerCmd_tb()
	NewNotebook/F=0/K=1/W=(378,188.75,638.25,282.5)/N=pasteFrom 
	Notebook pasteFrom text="Modifygraph arrowmarker={WD_arrowInfo, 0,0,0,1}"
End



//////////////////////////////////////////////   INITIALIZATION   /////////////////////////////////////////////////////////////////////////////////
Static Function lar_initPkgFldr()
	NewDataFolder/O root:Packages									// ensure a Packages subfolder exists
	NewDataFolder/O $"root:Packages:"+ksPackageSubfolder 			// ensure a subfolder for this package exists
	NewDataFolder/O $"root:Packages:"+ksPackageSubfolder+":menus" 	// ensure a subfolder for menus exists on load
End


// lar_populateLB( listWave, listSelWave, listType )
// 	listWave 	text wave reference to fill with wave names
// 	listSelWave 	numeric wave reference to fill with selection flags
//	listType 		type of items to populate list with
//		bit 0 	value: 1		numeric waves
//		bit 1		value: 2		text waves
//		bit 0&1	value: 3		numeric & text waves
//		bit 2		value: 4 		full datafolder paths (except Packages directory)
// 		
// returns 0 for success
// 
// populates provided selection wave with names of nontext waves in current datafolder
Static Function lar_populateLB( listWave, listSelWave, listType )
	WAVE/T listWave
	WAVE listSelWave
	variable listType
	
	variable i 											// counter
	string popList										// list of contents to place into current DF
	string preserve = ""									// keyworded list for preserving selection
	
	for (i=0; i<numpnts(listWave); i+=1)						// for each wave already in list
		preserve+= listWave[i]+":"+num2istr(listSelWave[i])+";" // add key:value; pair, key=wavename value=selected
	endfor
	Redimension/N=0 listWave, listSelWave 				// empty out waves
	
	switch (listType)										// depending on contents to list
		case 1:	// = 2^0 										// numeric waves only
			popList = WaveList("*",";","TEXT:0")						// list nontext waves
			break
		case 2:	// = 2^1 										// text waves only
			popList = WaveList("*",";","TEXT:1")						// list text waves
			break
		case 3: // = 2^0 + 2^1 									// text & numeric waves
			popList = WaveList("*",";","") 							// list all waves
			break
		case 4: // = 2^2										// full datafolder paths only
			popList = GetDataFolderList()							// same as used for drop-down menus
			break
		default: 												// unrecognized bit combinations
			popList = "" 											// get an empty list
	endswitch
	Redimension/N=(ItemsInList(popList)) listWave, listSelWave // resize to fit new list
	
	if ( ItemsInList(popList) > 0 )							// if found some results
		for (i=0; i<ItemsInList(popList); i+=1)					// for each found wave
			listWave[i] = StringFromList(i, popList)					// put into wave list
			if ( NumberByKey(listWave[i], preserve) & 0x01 )			// if prior state (looked up by wave's name) was selected
				listSelWave[i] = 1 									// set selected
			else													// if not found in prior state list
				listSelWave[i] = 0										// set unselected
			endif
		endfor
	endif
	return 0 												// return success
End





// lar_getGlobalSRef( strName, subfolder, defaultStr )
//	strName 	string containing name of global string to access
// 	subfolder 	subfolder within package datafolder; set empty string for none
// 	defaultStr 	string to set as default value in case global must be created
//
// returns string path to global string of name 'strName' 
// 
// Function first enters or creates folder 'Packages:lar_tools:xxxxxxx' where xxxxxxx 
// is determined by the 'subfolder' argument, then creates global string variable if not 
// existing and assigns default value 'defaultStr' 
Static Function/S lar_getGlobalSRef(strName, subfolder, defaultStr)
	string strName 								// name of global string to access
	string subfolder								// inside which subfolder of kPackageGlobalsPath
	string defaultStr								// value to give if not found
	string DFpath 								// path to string
	
	lar_initPkgFldr()										// ensure base packages folders are present
	DFpath = "root:Packages:"+ksPackageSubfolder 	// base folder directory
	if ( strlen(subfolder) > 0 ) 						// if subfolder supplied
		DFpath += ":"+PossiblyQuoteName(subfolder) 	// add to path
		NewDataFolder/O $DFpath						// ensure subfolder exists
	endif
		
	DFpath += ":"+strName						// append wave's name to path
	SVAR/Z foo = $DFpath 						// refer to global
	if ( !SVAR_exists(foo) )						// if doesn't exist
		string/G $DFpath = defaultStr					 // create it
	endif
	return DFpath								// return path
End

// lar_getGlobalVRef( varName, subfolder, defaultVal )
//	varName 	string containing name of global variable to access
// 	subfolder 	subfolder within package datafolder; set empty string for none
// 	defaultVal 	value to set as default in case global must be created
//
// returns string path to global string variable of name 'strName' 
// 
// Function first enters or creates folder 'Packages:lar_tools:xxxxxxx' where xxxxxxx 
// is determined by the 'subfolder' argument, then creates global numeric variable if not 
// existing and assigns default value 'defaultVal' 
Static Function/S lar_getGlobalVRef(varName, subfolder, defaultVal)
	string varName								// name of global var to access
	string subfolder								// inside which subfolder of kPackageGlobalsPath
	variable defaultVal							// value to give if not found
	string DFpath 								// full path to variable
	
	lar_initPkgFldr()										// ensure base packages folders exist
	DFpath = "root:Packages:"+ksPackageSubfolder 	// base folder
	if ( strlen(subfolder) > 0 )						// if subfolder was supplied
		DFpath += ":"+PossiblyQuoteName(subfolder) 	// add to path
		NewDataFolder/O $DFpath						// ensure subfolder exists
	endif

	DFpath += ":"+varName				// append variable name to path		
	NVAR/Z foo = $DFpath  				// make ref to global
	if ( !NVAR_exists(foo) ) 				// if doesn't exist
		variable/G $DFpath = defaultVal 		// create it
	endif	
	return DFpath 						// pass back path
End

// lar_getGlobalWRef( wName, subfolder, wType )
//	wName 		string containing name of wave to access
// 	subfolder 	subfolder within package datafolder; set empty string for none
// 	wType 		wave type to create if needed (sum values to specify; see WaveType/Y= for details
//				specify a negative value to use default (2=single precision numeric)
//						Value		Bit #	Hex Val		Type
//						0			-		-			text
//						1			0 		1			complex flag (used with any nontext type)
//	 					2 			1 		2			32-bit float, single prec
// 						4 			2 		4 			64-bit float, double prec
//						 8 			3 		8 			8-bit integer
// 						16 			4 		10 			16-bit integer
// 						32 			5 		20 			32-bit integer
//						64 			6 		40 			unsigned flag (used with any integer type)
//
// returns string path to wave of name 'wName' in packages subfolder 
// 
// Function first enters or creates folder 'Packages:lar_tools:xxxxxxx' where xxxxxxx 
// is determined by the 'subfolder' argument, then creates empty single-point wave, if one
// is not existing, using the provided wavetype code (single-precision numeric by default)
Static Function/S lar_getGlobalWRef( wName, subfolder, wType)
	string wName						// name of wave to access
	string subfolder						// inside which subfolder of kPackageGlobalsPath
	variable wType 						// code for making new wave if needed
	variable free							// boolean: nonzero for free waves
	string DFpath
	
	lar_initPkgFldr()								// ensure packages base folders exists
	DFpath = "root:Packages:"+ksPackageSubfolder // base folder
	if ( strlen(subfolder) > 0 )				// if subfolder provided
		DFpath += ":"+subfolder				// append to path
		NewDataFolder/O $DFpath				// ensure subfolder exists
	endif

	DFpath += ":"+wName				// append wave name to path	
	wave/Z foo = $DFpath 					// ref to wave
	if ( !WaveExists(foo) )					// if wave missing
		if ( wType < 0 )						// if negative (default)								// if not
			Make/N=0 $DFpath					// use igor defaults, length 0
		elseif ( wType == 0 )					// if zero (text wave)								// if not
			Make/T/N=0/Y=(wType) $DFpath		// add /T flag for auto reference ability
		elseif ( wType & 0x01 )				// if wave type code says complex								// if not
			Make/C/N=0/Y=(wType) $DFpath		// add /C flag for auto reference ability
		else									// otherwise								// if not
			Make/N=0/Y=(wType) $DFpath			// create only using type code
		endif	
	endif
	return DFpath 						// return path to wave
End








Static Function/DF SwitchToWorkFolder()
	return PromptSetDataFolder()
End








//Window lar_makeWSWDPanel_1() : Panel
//	PauseUpdate; Silent 1		// building window...
//	NewPanel /K=1 /W=(108,141,378,701)
//	SetDrawLayer UserBack
//	DrawLine 273,317,290,328
//	DrawLine 291,326,273,339
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 153,504,"points set to NAN"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 12,519,"Set 100% to disable"
//	PopupMenu inFolder,pos={10,9},size={250,21},bodyWidth=250,proc=makeWSWD_popup
//	PopupMenu inFolder,mode=1,popvalue="root:'2010.08.25_1hz':",value= #"lar_getFolderList()"
//	GroupBox sonicBox,pos={3,38},size={262,107},title="Sonic Data",frame=0
//	PopupMenu uPU,pos={11,59},size={141,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="U"
//	PopupMenu uPU,mode=1,popvalue="PHOTON_CNT",value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//	TitleBox Utext,pos={156,57},size={101,26},title="(+) = into sonic array,\r\t\t\tparallel to boom"
//	TitleBox Utext,frame=0
//	PopupMenu vPU,pos={12,88},size={140,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="V"
//	PopupMenu vPU,mode=1,popvalue="PHOTON_CNT",value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//	TitleBox Vtext,pos={159,90},size={93,13},title="right-handed w.r.t U",frame=0
//	SetVariable azimuth,pos={10,119},size={91,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Azimuth"
//	SetVariable azimuth,format="%u",limits={0,359,1},value= _NUM:0,live= 1
//	SetVariable avgperSV,pos={12,444},size={150,16},bodyWidth=60,proc=lar_tools#lar_wswdSetVProc,title="Window size (sec)"
//	SetVariable avgperSV,limits={1,86400,60},value= _NUM:300,live= 1
//	CheckBox evenIntCB,pos={172,445},size={82,14},proc=lar_tools#lar_wswdChkProc,title="Nice intervals"
//	CheckBox evenIntCB,value= 1
//	GroupBox ws,pos={5,152},size={260,107},title="Wind Speed Ouput Names",frame=0
//	CheckBox ws_VavgCB,pos={13,174},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_VavgCB,value= 0
//	SetVariable ws_VavgSV,pos={36,174},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Average:"
//	SetVariable ws_VavgSV,limits={-inf,inf,0},value= _STR:"WS_vctr_*"
//	TitleBox Utext5,pos={271,164},size={131,24},title="= (|U|\\Bmean\\M\\S2\\M + |V|\\Bmean\\M\\S2\\M)\\S1/2\\M"
//	TitleBox Utext5,frame=0
//	CheckBox ws_SavgCB,pos={13,195},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_SavgCB,value= 0
//	SetVariable ws_SavgSV,pos={37,195},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Average:"
//	SetVariable ws_SavgSV,limits={-inf,inf,0},value= _STR:"WS_sclr_*"
//	TitleBox Utext7,pos={272,190},size={117,20},title="= mean[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//	TitleBox Utext7,frame=0
//	CheckBox ws_SsdevCB,pos={13,214},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_SsdevCB,value= 0
//	SetVariable ws_SsdevSV,pos={38,215},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Std Dev:"
//	SetVariable ws_SsdevSV,limits={-inf,inf,0},value= _STR:"WS_sclr_*_sdev"
//	TitleBox Utext8,pos={272,212},size={117,20},title="= stdev[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//	TitleBox Utext8,frame=0
//	CheckBox ws_persCB,pos={13,234},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_persCB,value= 0
//	SetVariable ws_persSV,pos={55,234},size={202,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Persistence:"
//	SetVariable ws_persSV,limits={-inf,inf,0},value= _STR:"WS_pers_*"
//	TitleBox Utext6,pos={273,238},size={118,17},title="= |WS|\\Bvector\\M / WS\\Bscalar\\M"
//	TitleBox Utext6,frame=0
//	GroupBox wd,pos={3,268},size={262,127},title="Wind Direction Output Names"
//	GroupBox wd,frame=0
//	CheckBox wd_VavgCB,pos={13,290},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VavgCB,value= 0
//	SetVariable wd_VavgSV,pos={35,290},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Average:"
//	SetVariable wd_VavgSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*"
//	TitleBox Utext3,pos={274,287},size={137,17},title="= atan2(-|Ux|\\Bmean\\M,-|Uy|\\Bmean\\M)"
//	TitleBox Utext3,frame=0
//	CheckBox wd_SavgCB,pos={13,310},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_SavgCB,value= 0
//	SetVariable wd_SavgSV,pos={36,310},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Average:"
//	SetVariable wd_SavgSV,limits={-inf,inf,0},value= _STR:"WD_sclr_*"
//	CheckBox wd_SsdevCB,pos={13,330},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_SsdevCB,value= 0
//	SetVariable wd_SsdevSV,pos={38,330},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Std Dev:"
//	SetVariable wd_SsdevSV,limits={-inf,inf,0},value= _STR:"WD_sclr_*_sdev"
//	TitleBox Utext4,pos={294,321},size={72,13},title="Mitsuta method",frame=0
//	CheckBox wd_VsdevYCB,pos={14,351},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VsdevYCB,value= 0
//	SetVariable wd_VsdevYSV,pos={37,350},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Std Dev:"
//	SetVariable wd_VsdevYSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*_sdevY"
//	TitleBox Utext9,pos={274,351},size={89,13},title="Yamartino estimate",frame=0
//	CheckBox wd_VsdevMCB,pos={14,370},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VsdevMCB,value= 0
//	SetVariable wd_VsdevMSV,pos={37,370},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Std Dev:"
//	SetVariable wd_VsdevMSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*_sdevM"
//	TitleBox Utext0,pos={274,373},size={74,13},title="Mardia estimate",frame=0
//	SetVariable savepathSV,pos={34,466},size={221,16},bodyWidth=181,proc=lar_tools#lar_wswdSetVProc,title="Save in"
//	SetVariable savepathSV,limits={-inf,inf,0},value= _STR:":"
//	Button submit,pos={158,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="Make WS/WD"
//	Button submit,fStyle=1
//	SetVariable declination,pos={128,120},size={107,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Declination"
//	SetVariable declination,format="%.1f"
//	SetVariable declination,limits={-180,180,0.1},value= _NUM:0,live= 1
//	SetVariable wildcardSV,pos={11,422},size={243,16},bodyWidth=80,proc=lar_tools#lar_wswdSetVProc,title="Replace * wildcard in names with "
//	SetVariable wildcardSV,value= _STR:"1"
//	GroupBox outputGB,pos={5,402},size={260,123},title="Output Options"
//	Button formulaB,pos={19,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="Hide formulas"
//	CheckBox savepathCB,pos={12,467},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox savepathCB,value= 1
//	SetVariable maxemptySV,pos={10,489},size={139,16},bodyWidth=40,proc=lar_tools#lar_boxMeanSetVProc,title=" Intervals missing >="
//	SetVariable maxemptySV,format="%g%",limits={0,100,0},value= _NUM:100,live= 1
//EndMacro
//
//Window lar_makeWSWDPanel_2() : Panel
//	PauseUpdate; Silent 1		// building window...
//	NewPanel /K=1 /W=(108,141,526,701)
//	SetDrawLayer UserBack
//	DrawLine 273,317,290,328
//	DrawLine 291,326,273,339
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 153,504,"points set to NAN"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 12,519,"Set 100% to disable"
//	PopupMenu inFolder,pos={10,9},size={250,21},bodyWidth=250,proc=makeWSWD_popup
//	PopupMenu inFolder,mode=1,popvalue="root:'2010.08.25_1hz':",value= #"lar_getFolderList()"
//	GroupBox sonicBox,pos={3,38},size={262,107},title="Sonic Data",frame=0
//	PopupMenu uPU,pos={11,59},size={141,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="U"
//	PopupMenu uPU,mode=1,popvalue="PHOTON_CNT",value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//	TitleBox Utext,pos={156,57},size={101,26},title="(+) = into sonic array,\r\t\t\tparallel to boom"
//	TitleBox Utext,frame=0
//	PopupMenu vPU,pos={12,88},size={140,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="V"
//	PopupMenu vPU,mode=1,popvalue="PHOTON_CNT",value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//	TitleBox Vtext,pos={159,90},size={93,13},title="right-handed w.r.t U",frame=0
//	SetVariable azimuth,pos={10,119},size={91,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Azimuth"
//	SetVariable azimuth,format="%u",limits={0,359,1},value= _NUM:0,live= 1
//	SetVariable avgperSV,pos={12,444},size={150,16},bodyWidth=60,proc=lar_tools#lar_wswdSetVProc,title="Window size (sec)"
//	SetVariable avgperSV,limits={1,86400,60},value= _NUM:300,live= 1
//	CheckBox evenIntCB,pos={172,445},size={82,14},proc=lar_tools#lar_wswdChkProc,title="Nice intervals"
//	CheckBox evenIntCB,value= 1
//	GroupBox ws,pos={5,152},size={260,107},title="Wind Speed Ouput Names",frame=0
//	CheckBox ws_VavgCB,pos={13,174},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_VavgCB,value= 0
//	SetVariable ws_VavgSV,pos={36,174},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Average:"
//	SetVariable ws_VavgSV,limits={-inf,inf,0},value= _STR:"WS_vctr_*"
//	TitleBox Utext5,pos={271,164},size={131,24},title="= (|U|\\Bmean\\M\\S2\\M + |V|\\Bmean\\M\\S2\\M)\\S1/2\\M"
//	TitleBox Utext5,frame=0
//	CheckBox ws_SavgCB,pos={13,195},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_SavgCB,value= 0
//	SetVariable ws_SavgSV,pos={37,195},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Average:"
//	SetVariable ws_SavgSV,limits={-inf,inf,0},value= _STR:"WS_sclr_*"
//	TitleBox Utext7,pos={272,190},size={117,20},title="= mean[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//	TitleBox Utext7,frame=0
//	CheckBox ws_SsdevCB,pos={13,214},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_SsdevCB,value= 0
//	SetVariable ws_SsdevSV,pos={38,215},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Std Dev:"
//	SetVariable ws_SsdevSV,limits={-inf,inf,0},value= _STR:"WS_sclr_*_sdev"
//	TitleBox Utext8,pos={272,212},size={117,20},title="= stdev[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//	TitleBox Utext8,frame=0
//	CheckBox ws_persCB,pos={13,234},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox ws_persCB,value= 0
//	SetVariable ws_persSV,pos={55,234},size={202,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Persistence:"
//	SetVariable ws_persSV,limits={-inf,inf,0},value= _STR:"WS_pers_*"
//	TitleBox Utext6,pos={273,238},size={118,17},title="= |WS|\\Bvector\\M / WS\\Bscalar\\M"
//	TitleBox Utext6,frame=0
//	GroupBox wd,pos={3,268},size={262,127},title="Wind Direction Output Names"
//	GroupBox wd,frame=0
//	CheckBox wd_VavgCB,pos={13,290},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VavgCB,value= 0
//	SetVariable wd_VavgSV,pos={35,290},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Average:"
//	SetVariable wd_VavgSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*"
//	TitleBox Utext3,pos={274,287},size={137,17},title="= atan2(-|Ux|\\Bmean\\M,-|Uy|\\Bmean\\M)"
//	TitleBox Utext3,frame=0
//	CheckBox wd_SavgCB,pos={13,310},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_SavgCB,value= 0
//	SetVariable wd_SavgSV,pos={36,310},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Average:"
//	SetVariable wd_SavgSV,limits={-inf,inf,0},value= _STR:"WD_sclr_*"
//	CheckBox wd_SsdevCB,pos={13,330},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_SsdevCB,value= 0
//	SetVariable wd_SsdevSV,pos={38,330},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Scalar Std Dev:"
//	SetVariable wd_SsdevSV,limits={-inf,inf,0},value= _STR:"WD_sclr_*_sdev"
//	TitleBox Utext4,pos={294,321},size={72,13},title="Mitsuta method",frame=0
//	CheckBox wd_VsdevYCB,pos={14,351},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VsdevYCB,value= 0
//	SetVariable wd_VsdevYSV,pos={37,350},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Std Dev:"
//	SetVariable wd_VsdevYSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*_sdevY"
//	TitleBox Utext9,pos={274,351},size={89,13},title="Yamartino estimate",frame=0
//	CheckBox wd_VsdevMCB,pos={14,370},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox wd_VsdevMCB,value= 0
//	SetVariable wd_VsdevMSV,pos={37,370},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc,title="Vector Std Dev:"
//	SetVariable wd_VsdevMSV,limits={-inf,inf,0},value= _STR:"WD_vctr_*_sdevM"
//	TitleBox Utext0,pos={274,373},size={74,13},title="Mardia estimate",frame=0
//	SetVariable savepathSV,pos={34,466},size={221,16},bodyWidth=181,proc=lar_tools#lar_wswdSetVProc,title="Save in"
//	SetVariable savepathSV,limits={-inf,inf,0},value= _STR:":"
//	Button submit,pos={158,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="Make WS/WD"
//	Button submit,fStyle=1
//	SetVariable declination,pos={128,120},size={107,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Declination"
//	SetVariable declination,format="%.1f"
//	SetVariable declination,limits={-180,180,0.1},value= _NUM:0,live= 1
//	SetVariable wildcardSV,pos={11,422},size={243,16},bodyWidth=80,proc=lar_tools#lar_wswdSetVProc,title="Replace * wildcard in names with "
//	SetVariable wildcardSV,value= _STR:"1"
//	GroupBox outputGB,pos={5,402},size={260,123},title="Output Options"
//	Button formulaB,pos={19,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="Hide formulas"
//	CheckBox savepathCB,pos={12,467},size={16,14},proc=lar_tools#lar_wswdChkProc,title=""
//	CheckBox savepathCB,value= 1
//	SetVariable maxemptySV,pos={10,489},size={139,16},bodyWidth=40,proc=lar_tools#lar_boxMeanSetVProc,title=" Intervals missing >="
//	SetVariable maxemptySV,format="%g%",limits={0,100,0},value= _NUM:100,live= 1
//EndMacro
//
//Window boxMeanPanel1() : Panel
//	PauseUpdate; Silent 1		// building window...
//	NewPanel /K=1 /W=(189,86,422,452) as "Box Mean Calculator"
//	ShowTools/A
//	SetDrawLayer UserBack
//	SetDrawEnv fsize= 14
//	DrawText 10,22,"Box Means"
//	SetDrawEnv fstyle= 2
//	DrawText 163,199,"default: 300"
//	SetDrawEnv fsize= 11
//	DrawText 10,222,"Align to nice intervals"
//	SetDrawEnv fstyle= 2
//	DrawText 163,222,"default: ON"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 72,261,"* = old wave name"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 145,328,"points set to NAN"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 7,345,"Set 100% to disable"
//	PopupMenu inFolder,pos={7,29},size={220,21},bodyWidth=220,proc=lar_tools#lar_boxMeanPopupProc
//	PopupMenu inFolder,mode=2,popvalue="root:'2010.08.25_1hz':",value= #"lar_getFolderList()"
//	ListBox wavesLB,pos={7,64},size={217,109}
//	ListBox wavesLB,listWave=root:Packages:lar_tools:boxMean:avgList
//	ListBox wavesLB,selWave=root:Packages:lar_tools:boxMean:avgListSel,mode= 4
//	SetVariable winsizeSV,pos={8,184},size={146,16},proc=lar_tools#lar_boxMeanSetVProc,title="Window size (sec)"
//	SetVariable winsizeSV,limits={1,86400,60},value= root:Packages:lar_tools:boxMean:winSize
//	CheckBox winalignCB,pos={118,208},size={16,14},proc=lar_tools#lar_boxMeanCheckProc,title=""
//	CheckBox winalignCB,value= 1
//	SetVariable maskAvgSV,pos={10,230},size={215,16},bodyWidth=158,proc=lar_tools#lar_boxMeanSetVProc,title="Save mask"
//	SetVariable maskAvgSV,value= root:Packages:lar_tools:boxMean:maskAvg
//	CheckBox sdevCB,pos={8,267},size={139,14},proc=lar_tools#lar_boxMeanCheckProc,title="Include stdev; use mask: "
//	CheckBox sdevCB,value= 1
//	SetVariable maskSdevSV,pos={150,268},size={75,16},proc=lar_tools#lar_boxMeanSetVProc,title=" "
//	SetVariable maskSdevSV,value= root:Packages:lar_tools:boxMean:maskSdev
//	CheckBox saveinCB,pos={7,291},size={54,14},proc=lar_tools#lar_boxMeanCheckProc,title="Save in"
//	CheckBox saveinCB,value= 0
//	SetVariable savepathSV,pos={67,291},size={158,16},bodyWidth=158,proc=lar_tools#lar_boxMeanSetVProc,title=" "
//	SetVariable savepathSV,frame=0
//	SetVariable savepathSV,value= root:Packages:lar_tools:boxMean:savePath,noedit= 1
//	SetVariable maxemptySV,pos={3,314},size={139,16},bodyWidth=40,proc=lar_tools#lar_boxMeanSetVProc,title=" Intervals missing >="
//	SetVariable maxemptySV,format="%g%"
//	SetVariable maxemptySV,limits={0,100,0},value= root:Packages:lar_tools:boxMean:maxEmpty,live= 1
//	Button submit,pos={164,337},size={60,20},proc=lar_tools#lar_boxMeanBtnProc,title="Average"
//	Button submit,fStyle=1
//	SetWindow kwTopWin,hook=lar_tools#lar_boxMeanPanelHook
//EndMacro
//




//
//
////-------------------------------------------------------------------------------------------------------------------------------------------------------------
//// builds and displays nanFillPanel
//Function nanFillPanel() : Panel					// show in panel menu
//	DoWindow/F nanFillPanel							// attempt to bring to focus
//	if (V_flag != 0)											// if exists
//		return 0													// leave function
//	endif
//
//	STRUCT nanFillPrefs prefs							// invoke preferences variable
//	nanFillLoadPrefs(prefs)							// look up prefs
//	variable left = prefs.panel.left						// use shorter local names
//	variable top = prefs.panel.top
//	variable right = prefs.panel.right
//	variable bottom = prefs.panel.bottom
//
//	// access/create globals for SetVariable
//	NVAR gLowP = $lar_getGlobalVRef("lowP", "nanFill", prefs.lowP)
//	NVAR gHighP = $lar_getGlobalVRef("highP", "nanFill", prefs.highP)
//	NVAR gNum1 = $lar_getGlobalVRef("num1", "nanFill", prefs.num1)
//	NVAR gCmp1excl = $lar_getGlobalVRef("cmp1excl", "nanFill", prefs.cmp1excl)
//	NVAR gNum2 = $lar_getGlobalVRef("num2", "nanFill", prefs.num2)
//	NVAR gCmp2excl = $lar_getGlobalVRef("cmp2excl", "nanFill", prefs.cmp2excl)
//	wave/T nanList = $lar_getGlobalWRef("nanList", "nanFill", 0)
//	wave nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80)
//
//	NewPanel/W=(left,top,right,bottom)/K=1/N=nanFillPanel as "NAN Fill Tools"
//	SetWindow nanFillPanel, hook=lar_tools#nanFillPanelHook
//	SetDrawLayer UserBack
//	SetDrawEnv fsize= 14 
//	DrawText 16,22,"NAN Fill"
//	DrawText 11,187,"Index range to affect (inclusive):"
//	DrawText 9,236,"Set values to NAN when:"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 96, 281,"Do comparison"
//	PopupMenu inFolder,pos={4,25},size={250,21},bodyWidth=250,proc=lar_tools#nanFillPopupProc
//	PopupMenu inFolder,value= #"lar_tools#getFolderList()"
//	ListBox wavesLB, pos={4,54},size={250,112},mode=4,listWave=nanList,selWave=nanListSel
//	SetVariable lowpSV,pos={14,190},size={100,16},bodyWidth=100,proc=lar_tools#nanFillSetVProc,title=" "
//	SetVariable lowpSV,limits={0,inf,0},value=gLowP,format="%d"
//	SetVariable highpSV,pos={121,190},size={113,16},bodyWidth=100,proc=lar_tools#nanFillSetVProc,title="to"
//	SetVariable highpSV,limits={0,inf,0},value=gHighP,format="%d"
//	PopupMenu modePU,pos={4,239},size={250,21},bodyWidth=250,proc=lar_tools#nanFillPopupProc,mode=(prefs.mode+1)
//	PopupMenu modePU,value= #"\"all points;equal to #1;greater than #1;less than #1;between #1 and #2;outside #1 and #2;within #1 of #2;\""
//	SetVariable num1SV,pos={7,283},size={77,16},bodyWidth=60,proc=lar_tools#nanFillSetVProc,title="#1",limits={-inf,inf,0},value=gNum1
//	CheckBox num1CB,pos={100,284},size={66,14},title="exclusive",variable=gCmp1excl
//	SetVariable num2SV,pos={7,305},size={77,16},bodyWidth=60,proc=lar_tools#nanFillSetVProc,title="#2",limits={-inf,inf,0},value=gNum2
//	CheckBox num2CB,pos={100,305},size={66,14},title="exclusive",variable=gCmp2excl
//	Button submit,pos={193,291},size={50,20},proc=lar_tools#nanFillBtnProc,title="NANs!",fStyle=1
//
////	PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), lar_tools#getFolderList())) // set selection to current DF
//	lar_populateLB(nanList, nanListSel, 3) 			// fill listbox with waves in datafolder
//	if ( prefs.mode == 0 )	 								// if mode is "all points"
//		SetVariable num1SV,win=nanFillPanel,disable=2		// disable entry of both numbers
//		Checkbox num1CB,win=nanFillPanel,disable=2 		
//		SetVariable num2SV,win=nanFillPanel,disable=2 	
//		Checkbox num2CB,win=nanFillPanel,disable=2
//	elseif ( prefs.mode < 4 )								// or if not between/outside/within
//		SetVariable num2SV,win=nanFillPanel,disable=2		// only disable num2
//		Checkbox num2CB,win=nanFillPanel,disable=2
//	endif
//End	
//// preferences information for nanFillPanel
//Structure nanFillPrefs
//	uint32	version			// YYYYMMDD
//	STRUCT Rect panel		// left, top, right, bottom
//	uchar 	mode			// numeric type-code of comparison to make
//	double 	num1			// comparison # 1
//	uchar 	cmp1excl		// boolean: true=inclusive comparison
//	double 	num2			// comparison # 2
//	uchar 	cmp2excl		// boolean: true=inclusive comparison
//	uint32 	lowP			// lower indice boundary
//	uint32 	highP			// higher indice boundary
//EndStructure
//// restores preferences to nanFillPanel
//Static Function nanFillLoadPrefs(prefs, [reset])
//	STRUCT nanFillPrefs &prefs
//	variable reset							// optional: nonzero to reset to defaults
//	variable currentVersion = 20101015		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_nanFill, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.panel.left = 390						// dot notion to access substructure
//		prefs.panel.top = 120
//		prefs.panel.right = 650
//		prefs.panel.bottom = 449
//		
//		prefs.mode = 0							// default to fill all points
//		prefs.num1 = NAN						// no default value
//		prefs.cmp1excl = 0						// default inclusive comparisons
//		prefs.num2 = NAN						// no default value
//		prefs.cmp2excl = 0						// default inclusive comparisons
//		prefs.lowP = 0							// default lowest point
//		prefs.highP = inf							// default past end of wave
//		
//		nanFillSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves preferences for function
//Static Function nanFillSavePrefs(prefs)
//	STRUCT nanFillPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_nanFill, prefs
//	return (V_flag)
//End
//// resets preferences to default values
//Static Function nanFillResetPrefs()
//	STRUCT nanFillPrefs prefs
//	// lar_print(1, "Panel preferences reset for Hard Filter/NAN Fill")
//	return nanFillLoadPrefs(prefs, reset=1)
//End
//// handles preferences concerning panel appearance, position, etc
//Static Function nanFillPanelHook(infoStr)
//	string infoStr									// passed to function from window action
//	STRUCT nanFillPrefs prefs						// invoke function preferences
//	string event = StringByKey("EVENT",infoStr)		// see what happened to the window
//	strswitch (event)								// depending on string value
//		case "activate": 								// brought into focus
//			PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList())) // set popup to current DF
//			wave/T/Z nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//			wave/Z nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//			lar_populateLB(nanList, nanListSel, 3)		// refresh listbox of waves in datafolder
//		case "moved":								// if moved
//		case "resize":								// or resized
//			nanFillLoadPrefs(prefs)				// look up current prefs
//			GetWindow nanFillPanel wsize			// grab new position info
//			variable scale = ScreenResolution / 72 		// convert points to device units
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			nanFillSavePrefs(prefs)					// save prefs
//			break
//		case "deactivate":							// left focus
//		case "kill":									// about to be killed unless return(2)
//			break
//	endswitch
//	return 0
//End
//// handles all buttons on nanFillPanel
//Static Function nanFillBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			strswitch (ba.ctrlName)					// depending on which control called
//				case "submit":							//  submit button
//					ControlInfo/W=$ba.win inFolder				// get info from folder popup
//					string location = S_value					// save to local variable
//					ControlInfo/W=$ba.win lowpSV				// get info from lower index SV
//					variable lowP = V_value					// save locally
//					ControlInfo/W=$ba.win highpSV			// get info from higher index SV
//					variable highP = V_value					// save locally
//					ControlInfo/W=$ba.win modePU			// get info from mode drop-down box
//					variable mode = (V_value - 1)				// save locally & adjust to be zero-indexed
//					ControlInfo/W=$ba.win num1SV			// get info from num1 SetVariable
//					variable num1 = V_value					// save locally
//					ControlInfo/W=$ba.win num1CB			// get info from incl/excl checkbox
//					variable cmp1excl = V_value				// save locally
//					ControlInfo/W=$ba.win num2SV 			// get info from num2 SetVariable
//					variable num2 = V_value					// save locally
//					ControlInfo/W=$ba.win num2CB			// get info from incl/excl checkbox
//					variable cmp2excl = V_value				// save locally
//		
//					wave/T nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//					wave nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//					if ( sum(nanListSel) == 0 )					// if no waves selected
//						// lar_print(0, "lar_nanFill(): no waves were selected")
//						return 1									// return failure
//					else
//						Make/WAVE/N=(sum(nanListSel)) wavesList
//						variable i, j=0								// counters
//						for (i=0; i<numpnts(nanList); i+=1) 			// for each wave in list
//							if ( nanListSel[i] & 0x01 )					// if is selected
//								wavesList[j] = $nanList[i] 					// add ref to list
//								j+=1 									// advance output indice
//							endif
//						endfor
//						lar_nanFIll(wavesList,mode,lowP=lowP,highP=highP,num1=num1,cmp1excl=cmp1excl,num2=num2,cmp2excl=cmp2excl)
//						Killwaves/Z wavesList
//					endif
//					break								// end submit button
//				default:
//					// do nothing
//			endswitch
//			break									// end case mouse up
//		case -1:									// control being killed
//		case 1: 									// mouse down
//		case 3:									// mouse up outside control
//		case 4:									// mouse moved on control
//		case 5:									// mouse enter control
//		case 6:									// mouse leave control
//			break
//	endswitch								// end event code switch
//	return 0									// return success
//End
//// handles all check boxes on nanFillPanel
//Static Function nanFillCheckProc(ca) : CheckBoxControl
//	STRUCT WMCheckboxAction &ca		// checkbox structure passed in
//	switch (ca.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT nanFillPrefs prefs		// create prefs structure
//			nanFillLoadPrefs(prefs)			// poplate prefs
//			strswitch (ca.ctrlName)				// based on which checkbox
//				case "num1CB":						// exclusive comparison on num1?
//					prefs.cmp1excl = ca.checked			// save value to prefs
//					break
//				case "num2CB": 						// exclusive comparison on num2?
//					prefs.cmp2excl = ca.checked			// save value to prefs
//					break
//			endswitch
//			nanFillSavePrefs(prefs)			// store prefs
//			break
//		case -1:								// control being killed
//			break
//	endswitch
//	return 0
//End
//// handles all drop-down menus on nanFillPanel
//Static Function nanFillPopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa					// drop-down structure is passed in
//	switch (pa.eventCode)							// depending on event code
//		case 2: 										// mouse up
//			STRUCT nanFillPrefs prefs				// create prefs structure
//			nanFillLoadPrefs(prefs)						// invoke current settings
//			strswitch (pa.ctrlName)						// switch on which drop-down changed
//				case "inFolder":								// working directory
//					if ( DataFolderExists(pa.popStr) )				// if directory exists
//						SetDataFolder $pa.popStr						// switch to
//					else											// otherwise
//						// look up index # of current datafolder in directory list used by panels
//						variable itemNum = 1+WhichListItem(GetDataFolder(1), GetDataFolderList())
//						// reset the popup of the softSpikeFilterPanel to current datafolder
//						PopupMenu inFolder, win=$pa.win, mode=(itemNum)
//					endif
//					wave/T/Z nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//					wave/Z nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//					lar_populateLB(nanList, nanListSel, 3)		// refresh listbox of waves in folder
//					break
//				case "modePU":								// style of fill (all, comparison, etc)
//					switch (pa.popNum-1) 						// based on mode selection
//						case 0:										// all points
//							SetVariable num1SV,win=$pa.win,disable=2 // disable entry for both numbers
//							Checkbox num1CB,win=$pa.win,disable=2 		
//							SetVariable num2SV,win=$pa.win,disable=2 	
//							Checkbox num2CB,win=$pa.win,disable=2
//							break
//						case 1: 										// equal to
//						case 2: 										// greater than
//						case 3: 										// less than
//							SetVariable num1SV,win=$pa.win,disable=0 // restore entry for num1
//							Checkbox num1CB,win=$pa.win,disable=0 		
//							SetVariable num2SV,win=$pa.win,disable=2 // disable entry for num2
//							Checkbox num2CB,win=$pa.win,disable=2							
//							break
//						case 4: 										// between
//						case 5:										// outside
//						case 6:										// within
//							SetVariable num1SV,win=$pa.win,disable=0 // restore entry for both numbers
//							Checkbox num1CB,win=$pa.win,disable=0	
//							SetVariable num2SV,win=$pa.win,disable=0 	
//							Checkbox num2CB,win=$pa.win,disable=0
//							break
//					endswitch
//					prefs.mode = (pa.popNum-1)					// save to prefs
//				default:
//					// do nothing
//					break
//			endswitch
//			nanFillSavePrefs(prefs)					// save preference changes
//			break
//		case -1: 										// control being killed
//			break
//	endswitch									// end event code switch
//	return 0										// return success value
//End											// end pop-up handler
//// handles all variable input boxes on nanFillPanel
//Static Function nanFillSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv				// setvariable structure passed in
//	switch (sv.eventCode)							// switch based on what happened
//		case 1:										// mouse up
//		case 2: 										// enter key
//		case 3:										// live update
//			STRUCT nanFillPrefs prefs			// create prefs structure
//			nanFillLoadPrefs(prefs)				// populate prefs
//			strswitch (sv.ctrlName)					// depending on which control sent
//				case "lowpSV":							// lower index #
//					NVAR gLowP = $lar_getGlobalVRef("lowP", "nanFill", prefs.lowP) // global ref
//					prefs.lowP = gLowP						// save to prefs
//					break
//				case "highPSV":							// higher index #
//					NVAR gHighP = $lar_getGlobalVRef("highP", "nanFill", prefs.highP) // global ref
//					prefs.highP = gHighP						// save to prefs
//					break
//				case "num1SV":							// comparison # 1
//					NVAR gNum1 = $lar_getGlobalVRef("num1", "nanFill", prefs.num1) // global ref
//					prefs.num1 = gNum1						// save to prefs
//					break
//				case "num2SV":							// comparison # 2
//					NVAR gNum2 = $lar_getGlobalVRef("num2", "nanFill", prefs.num2) // global ref
//					prefs.num2 = gNum2						// save to prefs
//					break
//			endswitch
//			nanFillSavePrefs(prefs)					// store prefs
//			break
//		case -1:										// control being killed
//			break
//	endswitch
//	return 0
//End
//



//// Creates the GUI panel for LTloadFiles(...)
//Static Function LTloadFilesPanel() : Panel
//	DoWindow/F LTloadFilesPanel							// attempt to bring to focus
//	if (V_flag != 0)											// if exists
//		return 0													// leave function
//	endif	
//	
//	STRUCT loadFilesPrefs prefs						// invoke preferences
//	lar_tools#loadFilesLoadPrefs(prefs)						// populate prefs
//	
//	PauseUpdate; Silent 1		// building window...
//	NewPanel/N=LTloadFilesPanel/W=(prefs.win.left,prefs.win.top,prefs.win.right,prefs.win.bottom)/K=1 as "File Load [LAR Tools]"
//	SetWindow kwTopWin,hook(phook)=lar_tools#loadFilesPanelHook	
//	
//	// Options group box
//	GroupBox gb_options,pos={7,4},size={341,171},title="Options",frame=0	
//	PopupMenu pu_filetype,pos={15,26},size={278,21},bodyWidth=230,title="File type: "
//	PopupMenu pu_filetype,mode=prefs.fileTypeMode,value= #"\"Plain text (CSV);Plain text (TAB);CSI Long header (TOAB1);CSI Short header (TOA5);CSI whatever;Picarro G2301 [CO2/CH4/H2O];Picarro G1103 [NH3];Los Gatos DLT-100 [N2O/CO];Generic GPS (NMEA)\""
//	CheckBox cb_overwrite,pos={63,52},size={101,14},title="Overwrite existing",value=prefs.overwrite,proc=lar_tools#loadFilesChkProc
//	CheckBox cb_autoname,pos={63,69},size={66,14},title="Autoname",value=prefs.autoname,proc=lar_tools#loadFilesChkProc
//	CheckBox cb_quiet,pos={204,52},size={76,14},title="Quiet results",value=prefs.quiet,proc=lar_tools#loadFilesChkProc
//	CheckBox cb_serialTS,pos={14,91},size={298,14},title="Convert serial date values to date/time (Excel/DAQFactory)",value=prefs.serialTS,proc=lar_tools#loadFilesChkProc
//	CheckBox cb_textTS,pos={14,109},size={249,14},title="Convert text timestamps to date/time (LoggerNet)",value=prefs.textTS,proc=lar_tools#loadFilesChkProc
//	CheckBox cb_xscaling,pos={14,127},size={180,14},proc=lar_tools#loadFilesChkProc,title="Insert timestamps in wave x-scales",value=prefs.xscaling
//	
//	// Destination group box	
//	GroupBox gb_dest,pos={6,184},size={342,166},title="Destination",frame=0
//	PopupMenu pu_base,pos={13,204},size={110,21},title="Base target:",value= #"lar_tools#GetDataFolderList()",proc=lar_tools#loadFilesPopupProc
//	Button btn_setDF,pos={262,203},size={80,20},title="Use current",proc=lar_tools#loadFilesBtnProc
//	SetVariable sv_sub,pos={17,231},size={248,16},bodyWidth=193,title="Subfolder: ",limits={-inf,inf,0},value=_STR:prefs.destSub,proc=lar_tools#loadFilesSetVProc
//	CheckBox cb_derivesub,pos={71,251},size={195,14},title="Derive subfolder name from file name ",value=prefs.deriveSub,proc=lar_tools#loadFilesChkProc
//	TitleBox tb_relpath,pos={271,232},size={64,13},title="(relative path)",frame=0
//	TitleBox tb_path,pos={17,304},size={28,13},title="Path: ",frame=0
//	TitleBox tb_dest,pos={47,301},size={295,39},title="empty",fixedSize=1
//
//	// Buttons
//	Button btn_cancel,pos={6,356},size={60,20},title="Cancel",proc=lar_tools#loadFilesBtnProc
//	Button btn_loadfiles,pos={288,357},size={60,20},title="Do it",proc=lar_tools#loadFilesBtnProc
//
//	// Populate values	
//	PopupMenu pu_base,mode=(Max((1+WhichListItem(prefs.destBase, lar_tools#GetDataFolderList())), 1))
//	TitleBox tb_dest, title=prefs.destBase+prefs.destSub
//
//	// put string pref into global var for SetVariable access
//	//	SVAR setvariable = $lar_getGlobalSRef("setvariable", "template", prefs.setvariable)
//	//NewPanel/W=(prefs.panel.left,prefs.panel.top,prefs.panel.right,prefs.panel.bottom)/K=1/N=templatePanel as "Template"
//	//SetWindow templatePanel, hook(phook)=lar_tools#lar_templatePanelHook
//	//PopupMenu inFolder,value=lar_GetDataFolderList(),mode=(Max((1+WhichListItem(GetDataFolder(1),lar_GetDataFolderList())), 1))
//	//lar_populateLB( filterList, filterListSel )
//EndMacro
////
//// Structure definition for loadFilesPanel preferences
//Static Structure loadFilesPrefs
//	uint32			version						// YYYYMMDD
//	STRUCT Rect 	win 							// left, top, right, bottom
//	uint16 			fileTypeMode					// selection in file type drop-down
//	uchar			overwrite						// boolean: overwrite existing waves
//	uchar			autoname					// boolean: autoname waves
//	uchar			quiet						// boolean: suppress results to history
//	uchar			serialTS						// boolean: convert serial dates into igor date/time
//	uchar			textTS						// boolean: convert text timestamps into igor date/time
//	char				destBase[MAXCMDLEN]	// string: destination path base
//	char				destSub	[MAXCMDLEN]	// string: destination path subfolder 
//	uchar			deriveSub					// boolean: derive subfolder name based on file name
//	uchar			xscaling						// boolean: insert timestamps into x-scale of waves
//EndStructure
////
//// loadFilesLoadPrefs(prefs [reset])
////	prefs	loadFilesPrefs structure
////
//// optional parameters
////	reset	boolean: nonzero to restore default values
////	
//// Populates preference structure with defaults and optionally resets values
//Static Function loadFilesLoadPrefs(prefs, [reset])
//	STRUCT loadFilesPrefs &prefs
//	variable reset									// optional: nonzero to reset to defaults
//	variable currentVersion = 20110307				// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_loadFiles, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.win.left = 233						// dot notion to access substructure
//		prefs.win.top = 141
//		prefs.win.right = 588
//		prefs.win.bottom = 525
//		prefs.fileTypeMode = 1					// def: plain text, csv
//		prefs.overwrite = 1						// def: on
//		prefs.autoname = 1						// def: on
//		prefs.quiet = 0							// def: off
//		prefs.serialTS = 0							// def: off
//		prefs.textTS = 0							// def: off
//		prefs.destBase = "root:"					// def: root
//		prefs.destSub = ""						// def: (none)
//		prefs.deriveSub = 0						// def: off
//		
//		loadFilesSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
////
//Static Function loadFilesSavePrefs(prefs)
//	STRUCT loadFilesPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_loadFiles, prefs
//	return (V_flag)
//End
//// 
//Static Function loadFilesResetPrefs()
//	STRUCT loadFilesPrefs prefs
//	return loadFilesLoadPrefs(prefs, reset=1)
//End
////
//// Handles all events related to the LTfileLoadPanel
//Static Function loadFilesPanelHook(hs)
//	STRUCT WMWinHookStruct &hs				// window passed by ref
//	STRUCT loadFilesPrefs prefs					// invoke prefs
//	loadFilesLoadPrefs(prefs)							// populate
//	switch (hs.eventCode)							// what happened?
//		case 0: 										// activate
//		case 6:										// resized
//		case 12:										// moved
//			GetWindow $hs.winName wsize					// grab new position info
//			variable scale = ScreenResolution / 72			// scale points to device units
//			prefs.win.left = V_left * scale					// update coordinates
//			prefs.win.top = V_top * scale
//			prefs.win.right = V_right * scale
//			prefs.win.bottom = V_bottom * scale
//			loadFilesSavePrefs(prefs)						// save prefs
//			break
//	endswitch
//	return 0
//End
////
//// Handles button events for loadFilesPanel
//Static Function loadFilesBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			STRUCT loadFilesPrefs prefs			// invoke prefs
//			loadFilesLoadPrefs(prefs)				// populate
//			strswitch( ba.ctrlName )					// who called?
//				case "btn_setDF":						// "Set to Current Datafolder"
//					PopupMenu pu_base,mode=(Max((1+WhichListItem(GetDataFolder(1), lar_tools#GetDataFolderList())), 1)) // set popup mode # to index of this folder in list
//					ControlInfo/W=$ba.win pu_base 			// where was it set?
//					switch (V_value)							// if it was
//						case 3:
//						case 4:
//							break
//					endswitch
//					lar_tools#loadFilesRefreshDestPath(ba.win) 	// refresh
//					break
//				case "btn_loadFiles":							// submit
//					print "Not working yet"
//					break
//				case "btn_cancel":
//					DoWindow/K $ba.win
//					break
//			endswitch
//			loadFilesSavePrefs(prefs)				// retain changes
//			break
//	endswitch								// end event code switch
//	return 0									// return success
//End
////
//// handles all checkboxes on templatePanel
//Static Function loadFilesChkProc(cb) : CheckBoxControl
//	STRUCT WMCheckboxAction &cb		// checkbox structure passed in
//	switch (cb.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT loadFilesPrefs prefs		// create prefs structure
//			loadFilesLoadPrefs(prefs)			// poplate prefs
//			strswitch (cb.ctrlName)				// based on which checkbox
//				case "cb_overwrite":					// overwrite
//					prefs.overwrite = cb.checked
//					break
//				case "cb_autoname": 				// autoname
//					prefs.autoname = cb.checked
//					break
//				case "cb_quiet":						// quiet
//					prefs.quiet = cb.checked
//					break
//				case "cb_serialTS":					// convert serial TS
//					prefs.serialTS = cb.checked
//					break
//				case "cb_textTS":					// convert text TS
//					prefs.textTS = cb.checked
//					break
//				case "cb_deriveSub":					// derive subfolder names
//					prefs.deriveSub = cb.checked
//					break
//				case "cb_xscaling":					// insert timestamps in wave x scales
//					prefs.xscaling = cb.checked
//					break
//			endswitch
//			loadFilesSavePrefs(prefs)			// store prefs
//			break
//	endswitch
//	return 0
//End
//// handles all drop-down menus on templatePanel
//Static Function loadFilesPopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa					// drop-down structure is passed in
//	switch (pa.eventCode)							// depending on event code
//		case 2: 										// mouse up
//			STRUCT loadFilesPrefs prefs				// create prefs structure
//			loadFilesLoadPrefs(prefs)						// invoke current settings
//			strswitch (pa.ctrlName)							// switch on which drop-down changed
//				case "pu_base":									// destination target base
//					If (DataFolderExists(pa.popStr))					// if folder selected exists
//						prefs.destBase = pa.popStr						// save path to prefs
//						lar_tools#loadFilesRefreshDestPath(pa.win) 			// refresh display
//						return 0 											// exit successfully
//					else 											// if not
//						prefs.destBase = "root:"							// set default value
//						return -1 										// exit with error code
//					endif
//					break
//				case "popup2":									// popup 2
//					// do something else
//					break
//			endswitch
//			loadFilesSavePrefs(prefs)					// save preference changes
//			break
//	endswitch									// end event code switch
//	return 0										// return success value
//End											// end pop-up handler
//// handles all variable input boxes on templatePanel 
//Static Function loadFilesSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv					// setvariable structure passed in
//	switch (sv.eventCode)								// switch based on what happened
//		case 1:											// mouse up
//		case 2: 											// enter key
//		case 3:											// live update
//		case 6:											// value changed by dependency formula
//			STRUCT loadFilesPrefs prefs				// create prefs structure
//			loadFilesLoadPrefs(prefs)					// populate prefs
//			prefs.destSub = cleanupPath(sv.sval)			// save
//			SetVariable sv_destSub,value=_STR:prefs.destSub
//			lar_tools#loadFilesRefreshDestPath(sv.win) 		// refresh display
//			loadFilesSavePrefs(prefs)						// store prefs
//			break
//	endswitch
//	return 0
//End
//// handles refreshing the path expression box
//Static Function loadFilesRefreshDestPath( win )
//	string win									// name of window called from
//	ControlInfo/W=$win pu_base 					// where was it set?
//	string path = S_value
//	ControlInfo/W=$win sv_sub
//	If (strlen(S_value) > 0)
//		path += S_value
//	endif
//	TitleBox tb_dest, win=$win, title=path
//End




//// lar_template()
////	none
////
//// optional parameters
////	none
////
//// returns blah
//// 
//// descriptive text here
//Function lar_template()
//	// parameter variables
//	// local & global variables
//	// save location if changing data folders
//	// display start-up message + time() @ level 1
//	// display finished message + time() @ level 1
//	// return value (for return string, add /S flag to function def)
//	return 0
//End
//// builds and displays templatePanel
//Function lar_templatePanel() : Panel					// show in panel menu
//	DoWindow/F templatePanel							// attempt to bring to focus
//	if (V_flag != 0)											// if exists
//		return 0													// leave function
//	endif
//
//	STRUCT lar_templatePrefs prefs						// invoke preferences variable
//	lar_templateLoadPrefs(prefs)								// look up prefs
//	// put string pref into global var for SetVariable access
////	SVAR setvariable = $lar_getGlobalSRef("setvariable", "template", prefs.setvariable)
//	//NewPanel/W=(prefs.panel.left,prefs.panel.top,prefs.panel.right,prefs.panel.bottom)/K=1/N=templatePanel as "Template"
//	//SetWindow templatePanel, hook(phook)=lar_tools#lar_templatePanelHook
//	//PopupMenu inFolder,value=lar_GetDataFolderList(),mode=(Max((1+WhichListItem(GetDataFolder(1),lar_GetDataFolderList())), 1))
//	//lar_populateLB( filterList, filterListSel )
//End	
//// preferences structure for templatePanel
//Structure lar_templatePrefs
//	uint32			version						// YYYYMMDD
//	STRUCT Rect 	panel						// left, top, right, bottom
//	char 			strVar[MAX_OBJ_NAME] 		// strings stored as character arrays
//	char 			pathVar[MAXCMDLEN]
//	uchar 			boolVar						// booleans fit into a char (1 byte)
//	STRUCT CBSV 	option1		 				// checkbox/set variable combo
//	STRUCT CBSV 	option2
//EndStructure
//// restores preferences for templatePanel
//Static Function lar_templateLoadPrefs(prefs, [reset])
//	STRUCT lar_templatePrefs &prefs
//	variable reset							// optional: nonzero to reset to defaults
//	variable currentVersion = 00000000		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_template, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.panel.left = 0						// dot notion to access substructure
//		prefs.panel.top = 0
//		prefs.panel.right = 0
//		prefs.panel.bottom = 0
//		
//		prefs.strVar = "wave name"
//		prefs.pathVar = ":"
//		prefs.boolVar = 0
//		prefs.option1.checked = 0
//		prefs.option1.isString = 0
//		prefs.option1.numVal = 100
//		prefs.option2.checked = 1
//		prefs.option2.isString = 1
//		prefs.option2.strVal = "string"
//		
//		lar_templateSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves preferences for templatePanel
//Static Function lar_templateSavePrefs(prefs)
//	STRUCT lar_templatePrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_template, prefs
//	return (V_flag)
//End
//// resets preferences to default values
//Static Function lar_templateResetPrefs()
//	STRUCT lar_templatePrefs prefs
//	// lar_print(1, "Panel preferences reset for template")
//	return lar_templateLoadPrefs(prefs, reset=1)
//End
//// handles preferences concerning panel appearance, position, etc
//Static Function lar_templatePanelHook(s)
//	STRUCT WMWinHookStruct &s				// window passed by ref
//	STRUCT lar_templatePrefs prefs				// invoke function preferences
//	lar_templateLoadPrefs(prefs)					// look up current prefs
//	switch (s.eventCode)							// what happened?
//		case 0: 										// activate
//			PopupMenu inFolder,win=$s.winName,mode=(Max((1+WhichListItem(GetDataFolder(1), GetDataFolderList())),1))
//			//PopupMenu wavePopup,win=$s.winName,mode=(Max((1+WhichListItem(prefs.strVar, WaveList("*",";","TEXT:0"))), 1))
//			//wave/T/Z xList = $lar_getGlobalWRef("xList", "template", 0) // refer to wave list
//			//wave/Z xListSel = $lar_getGlobalWRef("xListSel", "template", 80) 	// refer to selection list
//			//lar_populateLB( xList, xListSel )				// refresh listbox of waves in folder
//		case 6:										// resized
//		case 12:										// moved
//			GetWindow $s.winName wsize				// grab new position info
//			variable scale = ScreenResolution / 72 			// convert points to device units
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			lar_templateSavePrefs(prefs)						// save prefs
//			break
//		case 1:		// deactivate
//		case 2:		// kill
//		case 3:		// mousedown
//		case 4:		// mousemoved
//		case 5:		// mouseup
//		case 7:		// cursormoved
//		case 8:		// modified (graph, notebook only)
//		case 9:		// enablemenu
//		case 10:		// menu
//		case 11:		// keyboard
//		case 13: 	// renamed
//		case 14:		// subwindowKill
//		case 15:		// hide
//		case 16: 	// show
//		case 17: 	// killVote
//		case 18: 	// showTools
//		case 19: 	// hideTools
//		case 20:		// showInfo
//		case 21: 	// hideInfo
//		case 22: 	// mouseWheel
//		case 23:		// spinUpdate (progress windows only)
//	endswitch
//	return 0
//End
//// handles all buttons on templatePanel
//Static Function lar_templateBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			STRUCT lar_templatePrefs prefs			// invoke prefs
//			lar_templateLoadPrefs(prefs)				// populate
//			strswitch( ba.ctrlName )					// who called?
//				case "one":								// 
//					break
//				case "submit":							// submit
//					break
//			endswitch
//			lar_templateSavePrefs(prefs)				// retain changes
//			break
//	endswitch								// end event code switch
//	return 0									// return success
//End
//// handles all checkboxes on templatePanel
//Static Function lar_templateCheckProc(cb) : CheckBoxControl
//	STRUCT WMCheckboxAction &cb		// checkbox structure passed in
//	switch (cb.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT lar_templatePrefs prefs		// create prefs structure
//			lar_templateLoadPrefs(prefs)			// poplate prefs
//			strswitch (cb.ctrlName)				// based on which checkbox
//				case "option1CB":					// option 1
//					prefs.option1.checked = cb.checked 	// store CB state to prefs
//					SetVariable option1SV,win=$cb.win,disable= (prefs.option1.checked ? 0 : 2) // enable/disable
//					break
//				case "option2CB": 					// option 2
//					prefs.option2.checked = cb.checked 	// store CB state to prefs
//					SetVariable option2SV,win=$cb.win,disable= (prefs.option2.checked ? 0 : 2) // enable/disable
//					break
//			endswitch
//			lar_templateSavePrefs(prefs)			// store prefs
//			break
//	endswitch
//	return 0
//End
//// handles all drop-down menus on templatePanel
//Static Function lar_templatePopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa					// drop-down structure is passed in
//	switch (pa.eventCode)							// depending on event code
//		case 2: 										// mouse up
//			STRUCT lar_templatePrefs prefs				// create prefs structure
//			lar_templateLoadPrefs(prefs)						// invoke current settings
//			strswitch (pa.ctrlName)							// switch on which drop-down changed
//				case "popup1":									// popup 1
//					// do something
//					break
//				case "popup2":									// popup 2
//					// do something else
//					break
//			endswitch
//			lar_templateSavePrefs(prefs)					// save preference changes
//			break
//	endswitch									// end event code switch
//	return 0										// return success value
//End											// end pop-up handler
//// handles all variable input boxes on templatePanel 
//Static Function lar_templateSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv				// setvariable structure passed in
//	switch (sv.eventCode)							// switch based on what happened
//		case 1:										// mouse up
//		case 2: 										// enter key
//		case 3:										// live update
//		case 6:										// value changed by dependency formula
//			STRUCT lar_templatePrefs prefs			// create prefs structure
//			lar_templateLoadPrefs(prefs)				// populate prefs
//			strswitch (sv.ctrlName)						// depending on which control sent
//				case "setVname1":							// 
//					break
//				case "setVname2":							//
//					break
//			endswitch
//			lar_templateSavePrefs(prefs)					// store prefs
//			break
//	endswitch
//	return 0
//End





/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////   WAVE FUNCTIONS   /////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

////lar_concatDFs( folders [, savePath, checkTime] )
////	folders 		wave containing datafolder references to operate on
////
//// optional parameters
////	savePath	string of output datafolder path (based from root:)
////	checkTime	boolean: non-zero verifies x-scale remains continuous across concatenation
////	killSrc		boolean: non-zero deletes source datafolders after concatenation
////
//// returns 0 for success
//// 
//// Function combines waves of the same name from datafolders provided in list, concatenating in the order
//// provided. Merged waves are dropped into root: unless an output path is specified via saveIn. Specifying a
//// nonzero value for checkTime causes function to verify the x-scale remains continuous (ie rightx(wave1)==leftx(wave2))
//Function lar_concatDFs( folders, [savePath, checkTime, killSrc])
//	WAVE/DF folders 								// wave containing datafolder references to operate on
//	string savePath 									// string containing full output path from root:
//	variable checkTime 								// boolean: non-zero verifies x-scaling remains continuous 
//	variable killSrc									// boolean: nonzero kills datafolders after appending
//	
//	variable i,j										// counters
//	variable numLists 								// number of rows of concatenation lists
//	variable numWaves								// number of waves found in each folder searched
//	variable labelIndex								// for looking up by row labels
//	variable timeOK									// boolean for checking time continuity
//	string name										// unique name creator
//	string wavesFound 								// searches for waves in each folder
//	string thisDF										// holds full path to current datafolder location
//	string thisList									// for analyzing concatenation lists
//	DFREF saveDFR = GetDataFolderDFR()				// save reference to current spot
//	
//	// lar_print(1, "Starting lar_concatDFs(...) at "+time())	// startup message
//	name = "Default settings loaded: "					// begin a default setting notice
//	If (ParamIsDefault(savePath))						// if not provided
//		savePath = ""									// blank for same directory
//		name += "saving output to current directory \""+GetDataFolder(1)+"\"; " // add to msg
//	else
//		if ( cmpstr(savePath[0], ":") )					// if first char not a colon
//			savePath = ":"+savePath						// append one
//		endif
//		savePath = RemoveEnding(savePath, ":")		// remove any trailing colons, if present
//		savePath += ":"								// append one so we know its there
//	endif
//	If (ParamIsDefault(checkTime))						// if not specified
//		// has default value of 0 
//		name += "not verifying continuous time (x-scaling); " // add to msg
//	endif
//	if ( strlen(name) > 30 )								// if msgs were appended
//		// lar_print(2, name)									// print default settings msg
//	endif
//	If (ParamIsDefault(killSrc))
//		killSrc = 0
//	endif
//	
//	name = UniqueName("wave", 1, 0)					// generate unique name for temp wave
//	Make/T/N=0 $name								// create empty text wave
//	WAVE/T concatLists = $name						// refer to
//	
//	// lar_print(2, "Building concatenation lists...")			// msg
//	for (i=0; i<numpnts(folders); i+=1) 					// for each provided DFREF
//		SetDataFolder folders[i] 							// move into datafolder
//		wavesFound = waveList("*",";","") 					// grab list of waves
//		numWaves = ItemsInList(wavesFound)				// how many waves were there?
//		numLists = numpnts(concatLists)					// how many spots are there?
//		thisDF = GetDataFolder(1) 						// grab string full path to this DF
//		if (numWaves > 0)								// if found some waves in this folder
//			// lar_print(3, ">\tIn DF "+thisDF+" found "+num2istr(numWaves)+" waves; list has "+num2istr(numLists)+" spots") // msg
//			if (numWaves > numLists)							// if more waves were found than lists there are
//				InsertPoints numLists, (numWaves-numLists), concatLists // add # of missing rows
//				// lar_print(2, "\tAdded "+num2istr(numWaves-numLists)+" rows for newly found waves") // msg
//				for (j=numLists; j<numpnts(concatLists); j+=1)			// for each newly created row 
//					name = StringFromList(j, wavesFound)				// get wave's name
//					SetDimLabel 0, j, $name, concatLists 				// set the label to wave's name
//					// lar_print(3, "\tSet a new dimension label: "+name) 	// msg
//				endfor
//				numLists = numpnts(concatLists)					// re-evaulate total # of lists to grab new end index
//			endif
//			for (j=0; j<numWaves; j+=1)						// for each wave found
//				name = StringFromList(j, wavesFound)				// look up its name
//				labelIndex = FindDimLabel(concatLists,0,name)		// look up its row index
//				if (labelIndex < 0)									// if not found
//					InsertPoints numLists, 1, concatLists 				// add row at end
//					SetDimLabel 0, numLists, $name, concatLists		// set new row's label to wave's name
//					// lar_print(2, "\tAdded a row & dim label for newly found wave "+name) // msg
//					numLists = numpnts(concatLists)					// recalcalate last index #
//					j -= 1											// push counter backwards then
//					continue											// skip remainder of loop to redo this row
//				endif
//				concatLists[FindDimLabel(concatLists,0,name)] += thisDF+name+";" // append full wave path to list
//			endfor
//			// lar_print(3, "\tFinished loading waves from "+thisDF+" into concatenation lists")
//		endif	
//	endfor
//	
//	SetDataFolder saveDFR 							// return to original datafolder
//	if ( checkTime )									// if will check times
//		// lar_print(2, "\tPerforming time (x-scale) continuity checks before concatenations") // msg
//	else 											// if not 
//		// lar_print(2, "\tPerforming concatenations without verifying time (x-scale) continuity") // msg
//	endif
//	for (i=0; i<numpnts(concatLists); i+=1)				// for each concatenation list built
//		thisList = concatLists[i] 							// make local to loop for ease
//		if ( checkTime ) 									// if supposed to verify continuous timing
//			timeOK = 1										// assume true
//			for (j=1; j<ItemsInList(thisList); j+=1)				// for each pair of waves in the list
//				wave A = $StringFromList(j-1, thisList) 				// build ref to earlier
//				wave B = $StringFromList(j, thisList)				// build ref to later
//				if ( (rightx(A)!=leftx(B)) || (deltax(A)!=deltax(B)) )		// if discontinuous or diff timestep
//					// lar_print(2, "*\tWaves "+GetWavesDataFolder(A,2)+" and "+GetWavesDataFolder(B,2)+" had discontinuous times and were not concatenated")
//					timeOK = 0										// display msg, set false
//					break											// leave j-for loop
//				endif
//			endfor
//			if ( !timeOK )										// if a time discontinuity was found
//				// lar_print(0, ">\tWaves of the name "+NameOfWave(A)+" were not concatenated because of time discontinuities.")
//				continue											// skip remainder of i-for loop
//			endif
//		endif			
//	
//		if ( strlen(savePath) > 0 )					// if a path was given
//			if ( !DataFolderExists(savePath) )			// if datafolder path is missing
//				NewDataFolder/O $RemoveEnding(savePath, ":") // create it 
//			endif
//		endif
//		name = PossiblyQuoteName(RemoveEnding(ParseFilePath(3, StringFromList(0, thisList), ":", 0, 0),";")) // look up wave name for this list
//		Concatenate/NP/O thisList, $(savePath+name) // do concat operation
//		// lar_print(3, "\tConcatenated waves of name "+name+" to wave "+savePath+name)
//	endfor
//	
//	If (killSrc)
//		for (i=0; i<numpnts(folders); i+=1)
//			KillDataFolder folders[i]
//		endfor
//	Endif
//	
//	Killwaves/Z concatLists
//	SetDataFolder saveDFR							// return user to original location
//	// lar_print(1, "Finished concatenating across datafolders at "+time()) // msg
//	return 0 											// signal success
//End
//
//// builds and displays concatDFsPanel
//Function lar_concatDFsPanel() : Panel					// show in panel menu
//	DoWindow/F concatDFsPanel							// attempt to bring to focus
//	if (V_flag != 0)											// if exists
//		return 0													// leave function
//	endif
//
//	STRUCT lar_concatDFsPrefs prefs						// invoke preferences variable
//	lar_concatDFsLoadPrefs(prefs)								// look up prefs
//
//	WAVE/T/Z dfList = $lar_getGlobalWRef("dfList", "concatDFs", 0) // create waves to hold listbox contents
//	WAVE/Z dfListSel = $lar_getGlobalWRef("dfListSel", "concatDFs", 80)
//	WAVE/T/Z concatList = $lar_getGlobalWRef("concatList", "concatDFs", 0) 
//	WAVE/Z concatListSel = $lar_getGlobalWRef("concatListSel", "concatDFs", 80)
//
//	// build panel window
//	NewPanel/W=(prefs.panel.left,prefs.panel.top,prefs.panel.right,prefs.panel.bottom)/K=1/N=concatDFsPanel as "Concatenate across datafolders"
//	SetWindow concatDFsPanel, hook(phook)=lar_tools#lar_concatDFsPanelHook
//	// add annotation
//	SetDrawLayer UserBack
//	SetDrawEnv fsize= 14
//	DrawText 5,20,"Concatenate across datafolders"
//	DrawText 12,46,"Select source folders"
//	DrawText 11,270,"Concat list"
//	// listbox of available data folders
//	ListBox foldersLB,pos={7,49},size={260,193},mode= 4,listWave=dfList,selWave=dfListSel
//	// listbox of selected data folders
//	ListBox selectionLB,pos={7,271},size={261,154},mode=4,listWave=concatList,selWave=concatListSel
//	// button to add folders to selection
//	Button addBtn,pos={156,246},size={110,20},proc=lar_tools#lar_concatDFsBtnProc,title="Add selection to list"
//	// button to remove folders from selection
//	Button remBtn,pos={128,430},size={140,20},proc=lar_tools#lar_concatDFsBtnProc,title="Remove selection from list"
//	// checkbox decides whether times are checked
//	CheckBox checkTimeCB,pos={4,483},size={161,14},title="Check time (x-scale) continuity",value=prefs.checkTime
//	CheckBox checkTimeCB,proc=lar_tools#lar_concatDFsCheckProc
//	// entry for target directory
//	SetVariable savepathSV,pos={6,459},size={264,16},bodyWidth=224,proc=lar_tools#lar_concatDFsSetVProc,title="Save in"
//	SetVariable savepathSV,limits={-inf,inf,0},value=_STR:prefs.savePath
//	// submit button
//	Button submit,pos={179,481},size={90,20},proc=lar_tools#lar_concatDFsBtnProc,title="Concatenate",fStyle=1
//	
//	lar_populateLB(dfList, dfListSel, 4) 			// populate available folders list with datafolders
//End	
//// preferences structure for concatDFsPanel
//Structure lar_concatDFsPrefs
//	uint32			version						// YYYYMMDD
//	STRUCT Rect 	panel						// left, top, right, bottom
//	char 			savePath[400]//MAXCMDLEN] 	// strings stored as character arrays
//	uchar 			checkTime					// booleans fit into a char (1 byte)
//EndStructure
//// restores preferences for concatDFsPanel
//Static Function lar_concatDFsLoadPrefs(prefs, [reset])
//	STRUCT lar_concatDFsPrefs &prefs
//	variable reset							// optional: nonzero to reset to defaults
//	variable currentVersion = 20101015		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_concatDFs, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion			// fill them with this version
//		prefs.panel.left = 101					// dot notion to access substructure
//		prefs.panel.top = 90
//		prefs.panel.right = 375
//		prefs.panel.bottom = 597
//		prefs.savePath = ":mergedDFs" 		// default name
//		prefs.checkTime = 1					// default to on
//		
//		lar_concatDFsSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves preferences for textToIgorTime function
//Static Function lar_concatDFsSavePrefs(prefs)
//	STRUCT lar_concatDFsPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_concatDFs, prefs
//	return (V_flag)
//End
//// resets preferences to default values
//Static Function lar_concatDFsResetPrefs()
//	STRUCT lar_concatDFsPrefs prefs
//	// lar_print(1, "Panel preferences reset for Concatenate across Datafolders")
//	return lar_concatDFsLoadPrefs(prefs, reset=1)
//End
//// handles preferences concerning panel appearance, position, etc
//Static Function lar_concatDFsPanelHook(s)
//	STRUCT WMWinHookStruct &s 				// caller passed by ref
//	switch (s.eventCode)							// what happened?
//		case 0:		 								// activate
//			wave/T/Z dfList = $lar_getGlobalWRef("dfList", "concatDFs", 0) // refer to wave list
//			wave/Z dfListSel = $lar_getGlobalWRef("dfListSel", "concatDFs", 80) // refer to selection list
//			lar_populateLB(dfList, dfListSel, 4)				// refresh listbox of datafolders
//		case 6:										// resized
//		case 12:										// moved
//			STRUCT lar_concatDFsPrefs prefs				// invoke function preferences
//			lar_concatDFsLoadPrefs(prefs)					// look up current prefs
//			GetWindow concatDFsPanel wsize				// grab new position info
//			variable scale = ScreenResolution / 72 			// convert points to device units
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			lar_concatDFsSavePrefs(prefs)					// save prefs
//			break
//	endswitch
//	return 0
//End
//// handles all buttons on concatDFsPanel
//Static Function lar_concatDFsBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			STRUCT lar_concatDFsPrefs prefs			// invoke prefs structure
//			lar_concatDFsLoadPrefs(prefs)				// populate them
//			variable i									// counter
//			wave/T dfList = $lar_getGlobalWRef("dfList", "concatDFs", 0) // build refs to listbox waves
//			wave dfListSel = $lar_getGlobalWRef("dfListSel", "concatDFs", 80)
//			wave/T concatList = $lar_getGlobalWRef("concatList", "concatDFs", 0)
//			wave concatListSel = $lar_getGlobalWRef("concatListSel", "concatDFs", 80)
//			strswitch( ba.ctrlName )				// depending on sender button
//				case "addBtn": 						// add selection to list
//					for (i=0; i<numpnts(dfList); i+=1) 		// for each point in source list
//						if ( dfListSel[i] & 0x01 )				// if currently selected
//							InsertPoints numpnts(concatList), 1, concatList, concatListSel // add 1 row to end of waves
//							concatList[numpnts(concatList)-1] = dfList[i] // add name to list
//						endif
//					endfor
//					break
//				case "remBtn": 						// remove selection from list
//					for (i=0; i<numpnts(concatList); i+=1)	// for each point in concat list
//						if ( concatListSel[i] & 0x01 )			// if currently selected
//							DeletePoints i, 1, concatList, concatListSel // remove this row from waves
//							i -= 1								// scoot counter back since rows shifted
//						endif
//					endfor
//					break
//				case "submit":						// submit button
//					if ( numpnts(concatList) == 0 )			// if no waves are in listbox
//						// lar_print(0, "No datafolders were specified in the concatenation list")
//						return -1								// quit
//					else
//						string name = UniqueName("wave", 1, 0) // create unique wave for DF refs
//						Make/O/DF/N=(numpnts(concatList)) $name // create temp wave
//						wave/DF folderList = $name			// refer to it
//						for (i=0; i<numpnts(concatList); i+=1)	// for each point in concat list
//							DFREF tmpRef = $concatList[i] 		// convert string to ref 
//							folderList[i] = tmpRef					// add to list
//						endfor
//						
//						//print "checktime: ",prefs.checktime
//						lar_concatDFs(folderList, savePath=prefs.savePath, checkTime=prefs.checkTime) // call function
//						Killwaves/Z folderList					// remove after done
//					endif
//					break
//			endswitch
//			break									// end case mouse up
//	endswitch								// end event code switch
//	return 0									// return success
//End
//// handles all checkboxes on concatDFsPanel
//Static Function lar_concatDFsCheckProc(ca) : CheckBoxControl
//	STRUCT WMCheckboxAction &ca		// checkbox structure passed in
//	switch (ca.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT lar_concatDFsPrefs prefs		// create prefs structure
//			lar_concatDFsLoadPrefs(prefs)			// poplate prefs
//			strswitch (ca.ctrlName)				// based on which checkbox
//				case "checkTimeCB":					// verify continuous time in x-scaling?
//					prefs.checkTime = ca.checked 		// save to prefs
//					print "checktime: ",prefs.checkTime
//					break
//			endswitch
//			lar_concatDFsSavePrefs(prefs)			// store prefs
//			break
//	endswitch
//	return 0
//End
//// handles all variable input boxes on concatDFsPanel 
//Static Function lar_concatDFsSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv				// setvariable structure passed in
//	switch (sv.eventCode)							// switch based on what happened
//		case 1:										// mouse up
//		case 2: 										// enter key
//		case 3:										// live update
//		case 6:										// value changed by dependency formula
//			STRUCT lar_concatDFsPrefs prefs				// create prefs structure
//			lar_concatDFsLoadPrefs(prefs)					// populate 
//			strswitch (sv.ctrlName)						// depending on which control sent
//				case "savepathSV":							// save in location
//					//SVAR gSavePath = $lar_getGlobalSRef("savePath", "concatDFs", prefs.savePath) // global ref
//					//gSavePath = lar_cleanupPath( gSavePath )		// format appropriately
//					//prefs.savePath = gSavePath 						// store to prefs
//					prefs.savePath = cleanupPath( sv.sval ) 	// reformat & save string entry 
//					break
//			endswitch
//			lar_concatDFsSavePrefs(prefs)					// store prefs
//			break
//	endswitch
//	return 0
//End





//// lar_softSpikeFilter( waves [, saveAs, winSize, multBase, multIncr, multPass] )
////
//// parameters
//// 	waves 		wave containing references to target waves
////
//// optional parameters
//// 	saveAs		mask for destination wave names; *=wave's name (default: same/overwrite)
////				done using stringreplace("*" with NameOfWave(wave) ) or similar
////	winSize 		window to apply to filter, seconds (default: 900=15min)
//// 	winAlign 	boolean align windows to whole intervals (default: nonzero = true)
////	multBase 	first-pass multiplier (default: 3.6)
////	multincr		multiplier increment per-pass (default: 0.3)
////	multPass	number of passes (default: 3)
////
//// returns a 0 for successfully finished
////
//// using default parameters, function applies a soft filter to each wave provided in the list,
//// and saves over the old wave. Providing a string name can prevent overwriting and any instance
//// of the asterisk * is replaced by the name of the old wave. The filter with default settings follows
//// the method selected in isoflux_02 (snp) which comes from HaPe Schmid:
//// 	"For each 15min period and variable, the means and variances are calculated. From these
//// 	diagnostics, a threshold for spikes is determined as a multiple of the standard deviation (3.6 S.D. 
//// 	initially, increased by 0.3 after each pass). On each pass, a soft spike is registered if the
//// 	fluctuation from the mean is larger than the threshold value, and if the duration of the spike is three
////	or fewer records, corresponding to a persistence of 0.3s, for the 10Hz sampling rate. Longer-lasting
//// 	departures from the period mean are taken to indicate possible physical events. After each pass, 
//// 	if spikes are detected, the mean and variance are adjusted to exclude data marked as spikes and 
//// 	the process repeated, until either there are no more new spikes or the maximum or three iterations 
//// 	is completed (which is rarely the case)."
//// Schmid, HaPe, C. Susan B. Grimmond, Ford Cropley, Brian Offerle, and Hong-Bing Su. "Measurements 
//// of CO2 and energy fluxes over a mixed hardwood forest in the mid-western United States." Agricultural 
//// and Forest Meteorology. 103 (2000): 357-374. Print.
//Function lar_softSpikeFilter( waves, [saveAs, winSize, winAlign, multBase, multIncr, multPass]) 
//	wave/WAVE waves						// wave containing references to target waves
//	string saveAs							// optional name to save waves as. instances of the asterisk *
//											// are replaced with the old waves name (default: overwrite)
//	variable winSize 							// optional window size to apply, sec (default: 900=15min)
//	variable winAlign							// optional boolean flag, align time windows to whole intervals
//	variable multBase 						// optional base multiplier (default: 3.6)
//	variable multIncr 							// optional per-pass multiplier increment (default: 0.3)
//	variable multPass 						// optional number of passes (default: 3)
//	
//	variable i,j,k,m 							// counters
//	variable mult								// working multiplier value
//	variable x0, x1, xn, dx, p0, p1				// time at point 0, 1, working points, delta (x-scale values)
//	variable avg, sdev							// results of wavestats op
//	variable remNANs	, totPnts, gone			// number of empty records removed prior to wavestats
//	variable above							// threshold flag
//	string name  								// name of current working wave
//	string saveAsBase 						// preserves original saveAs value
//	
//	// lar_print(1, "Starting lar_softSpikeFilter(...) at "+time()) // msg
//	name = "Default settings loaded for: "				// borrow name to output a verbose message
//	if ( ParamIsDefault(saveAs) ) 				// if no name given,
//		name += "overwrite ON; " 			// add to verbose message
//		saveAsBase = "*"							// set default, * = old name
//	else 									// if there was
//		saveAsBase = saveAs					// preserve original value
//	endif
//	if ( ParamIsDefault(winSize) )				// if no window size,
//		name += "window size; " 					// add to verbose msg
//		winSize = 900							// set default
//	endif
//	if ( ParamIsDefault(winAlign) )				// if no align boolean,
//		name += "window alignment; "				// add to verbose msg
//		winAlign = 1								// set true
//	endif
//	if ( ParamIsDefault(multBase) )				// if no base multipler
//		name += "base multiplier; "				// add to verbose msg
//		multBase = 3.6							// set default
//	endif
//	if ( ParamIsDefault(multIncr) )				// if no increment set
//		name += "multiplier increment; " 			// add to verbose msg
//		multIncr = 0.3							// set default
//	endif
//	if ( ParamIsDefault(multPass) )				// if number passes not set
//		name += "# of passes; "					// add to verbose msg
//		multPass = 3							// set default
//	endif
//	if ( strlen(name) > 30 )						// if default settings appended 
//		// lar_print(2, name)							// if verbose, print default settings message
//	endif
//	Make/T/N=(numpnts(waves),2) names0 	// make summary wave for names
//	Make/N=(numpnts(waves),multPass,2) rem0 // make summary wave for spikes & points removed per pass
//	
//	for (i=0; i<numpnts(waves); i+=1)			// for each wave in list
//		wave/Z src = waves[i]						// grab ref from list
//		if ( !WaveExists(src) || WaveType(src)==0 || WaveDims(src)!=1 ) // if missing, text or multidim
//			// lar_print(0,"Wave \""+NameOfWave(src)+"\" was missing, text or multidimensional. Skipping...")
//			continue									// skip rest of this for loop
//		endif
//		if ( cmpstr(WaveUnits(src, 0), "dat") )			// if wave x-scaling isn't in date/time units
//			// lar_print(0,"Wave \""+NameOfWave(src)+"\" scaling inappropriate; date/time is expected. Skipping...")
//			continue 								// skip this wave too
//		endif
//		name = NameOfWave(src)
//		saveAs = ReplaceString("*", saveAsBase, name) // change final name before shifting refs
//		names0[i][0] = name 						// store original wave name 
//		names0[i][1] = saveAs 					// store output wave name
//		// lar_print(2, "Working on wave "+name) 		// msg
//		
//		name = UniqueName("wave", 1, 0) 			// create unique name
//		Duplicate/O src, $name					// create copy of working wave
//		wave src = $name						// change reference to copy
//		
//		for (j=0; j<multPass; j+=1)					// for each pass 
//			mult = multBase + j*multIncr				// calculate multiplier value
//			// lar_print(2, "=\tBeginning pass number "+num2istr(j+1)+" using multiplier "+num2str(mult)) // msg
//			rem0[i][j][0] = 0							// set spikes counter @ zero
//			rem0[i][j][1] = 0							// set points counter @ zero
//			x0 = leftx(src) 							// grab x-value at point 0
//			if (winAlign) 								// if flag to align windows is true
//				x0 = trunc(x0 / winSize)*winSize			// round to next lowest whole interval of winSize
//			endif
//			dx = deltax(src)							// make local var to be faster
//			do										// enter indefinite loop
//				x1 = x0+winSize	 						// calculate x-value at start of next window
//				name = UniqueName("wave", 1, 0)			// find unique temp name
//				Duplicate/O/R=(x0,(x1-dx)) src, $name 		// duplicate chunk of time to temp wave
//				wave/Z work = $name						// make reference to chunk
//				totPnts = numpnts(work)					// how many points?
//				remNANs = RemoveNANs(work)			// remove NAN points
//				gone = 100*(remNANs/totPnts)				// calculate % points missing
//				if ( gone >= 100 )							// if all were gone
//					// lar_print(3,"\t"+secs2time(x0, 3, 2)+" to "+secs2time(x1-dx, 3, 2)+" has no data ("+num2istr(totPnts)+" points)... skipping interval")
//					Killwaves work							// delete temp wave
//					x0 = x1									// shift left window point to next window
//					if ( x0 >= rightx(src) )						// if next window would be past end of wave
//						break									// exit loop
//					else
//						continue									// start this do-loop over
//					endif
//				else 									// if not
//					WaveStats/Q work						// do wavestats for this chunk of time
//					avg = V_avg								// save result
//					sdev = V_sdev							// save result
//					// lar_print(3,"\t"+secs2time(x0, 3, 2)+" to "+secs2time(x1-dx, 3, 2)+" avg: "+num2str(avg)+" sdev: "+num2str(sdev)+"; "+num2istr(remNANs)+" NANs removed ("+num2str(gone)+"%)")
//				endif
//				
//				above = 0								// reset before loop
//				for ( k=x0; k<x1; k+=dx )					// for each x-value
//					if ( abs(src(k) - avg) > abs(mult*sdev) )		// if fluctuation > threshold
//						// lar_print(3, "\t\tExceedance at "+secs2time(k, 3, 2)+": "+num2str(abs(src(k) - avg))+" > "+num2str(abs(mult*sdev)))
//						if (!above)								// if flag was low
//							xn = k									// mark as starting time
//							// lar_print(4, "\tSetting high point at "+secs2time(k, 3, 2)) // debug
//							above = 1								// set threshold flag hi
//						endif
//					elseif (above)								// if fluctuation < threshold but was just >
//						// lar_print(4, "\tReturned low at "+secs2time(k, 3, 2)) // debug
//						above = 0								// set flag low again
//						if ( (k - xn) < 0.3 )							// if duration was < 0.3 sec
//							p0 = x2pnt(src, xn) 						// look up point at start
//							p1 = x2pnt(src, k-dx)						// look up point at time of last loop
//							src[p0,p1] = NAN							// set records in copy blank
//							rem0[i][j][0] += 1							// add to spike counter
//							rem0[i][j][1] += (p1-p0+1) 					// add # points to counter
//							// lar_print(2,"*\tSpike at "+secs2time(xn, 3, 2)+" containing "+num2istr(p1-p0+1)+" points; replaced with NANs")
//						endif
//					endif
//				endfor
//				
//				avg = NAN 								// clear out variables b4 next interval
//				sdev = NAN
//				Killwaves work							// delete temp copy
//				x0 = x1									// set left window point to next point
//			while ( x0<rightx(src) )					// until left window point leaves wave
//			// lar_print(2, ">\tRemoved "+num2istr(rem0[i][j][0])+" spikes containing "+num2istr(rem0[i][j][1])+" points")
//		endfor//each pass
//		
//		// lar_print(2,"Saving to final wave name "+saveAs) // msg
//		Duplicate/O src, $saveAs 				// copy to final name
//		Killwaves src							// delete working copy
//	endfor//each wave in list
//	
//	// lar_print(1, "Finished applying soft spike filter at "+time()) // start summary message
//	for (i=0; i<DimSize(names0, 0); i+=1)		// for each wave worked on
//		name = "\t"+names0[i][0]+"\t\t"+names0[i][1] // add names to summary line
//		avg = 0									// borrow var to track total spikes removed
//		sdev = 0									// borrow to track total points removed
//		for (j=0; j<DimSize(rem0, 1); j+=1)			// for each pass
//			name += "\t\t"+num2istr(rem0[i][j][0])+"/"+num2istr(rem0[i][j][1]) // print spikes/points removed
//			avg += rem0[i][j][0]						// add to spike total
//			sdev += rem0[i][j][1]						// add to pnt total
//		endfor
//		name += "\t\t("+num2istr(avg)+"/"+num2istr(sdev)+")"
//		// lar_print(1, name)							// build summary line & print it		
//	endfor
//	
//	Killwaves names0, rem0						// remove summary waves
//	return 0										// exit code
//End	
//// builds and displays the softSpikeFilterPanel
//Function lar_softSpikeFilterPanel() : Panel
//	DoWindow/F softSpikeFilterPanel		// attempt to bring into focus
//	if ( V_flag == 1)						// if window was found
//		return 0								// quit
//	endif
//	
//	STRUCT lar_softSpikeFilterPrefs prefs 	// create prefs structure
//	lar_softSpikeFilterLoadPrefs(prefs)		// invoke pref values
//	variable left = prefs.panel.left			// populate panel location locally
//	variable top = prefs.panel.top 
//	variable right = prefs.panel.right
//	variable bottom = prefs.panel.bottom
//	
//	// access/create global preference variables
//	NVAR gWinSize = $lar_getGlobalVRef("winSize", "softSpikeFilter", prefs.winSize)
//	NVAR gWinAlign = $lar_getGlobalVRef("winAlign", "softSpikeFilter", prefs.winAlign)
//	NVAR gMultBase = $lar_getGlobalVRef("multBase", "softSpikeFilter", prefs.multBase)
//	NVAR gMultIncr = $lar_getGlobalVRef("multIncr", "softSpikeFilter", prefs.multIncr)
//	NVAR gMultPass = $lar_getGlobalVRef("multPass", "softSpikeFilter", prefs.multPass)
//	SVAR gSaveAs = $lar_getGlobalSRef("saveAs", "softSpikeFilter", prefs.saveAs)
//	wave/T filterList = $lar_getGlobalWRef("filterList", "softSpikeFilter", 0) 
//	wave filterListSel = $lar_getGlobalWRef("filterListSel", "softSpikeFilter", 80)
//	
//	// build new panel
//	NewPanel/W=(left,top,right,bottom)/K=1/N=softSpikeFilterPanel as "Soft Spike Filter"
//	SetWindow softSpikeFilterPanel, hook=lar_tools#lar_softSpikeFilterPanelHook
//	// draw text annotation
//	SetDrawLayer UserBack
//	SetDrawEnv fsize= 14
//	DrawText 10,23,"Soft Spike Filter "
//	DrawText 10,74,"Select wave(s) to filter:"
//	SetDrawEnv fstyle= 2
//	DrawText 72,232,"* = old wave name"
//	SetDrawEnv fstyle= 2
//	DrawText 162,292,"default: 900"
//	SetDrawEnv fsize= 11
//	DrawText 13,310,"Align to nice intervals"
//	SetDrawEnv fstyle= 2
//	DrawText 162,310,"default: ON"
//	SetDrawEnv fstyle= 2
//	DrawText 161,331,"default: 3.6"
//	SetDrawEnv fstyle= 2
//	DrawText 162,353,"default: 0.3"
//	SetDrawEnv fstyle= 2
//	DrawText 161,374,"default: 3"
//	// select working datafolder via drop-down
//	PopupMenu inFolder, win=softSpikeFilterPanel, pos={8,33}, size={222,21}, bodyWidth=222
//	PopupMenu inFolder, popvalue=GetDataFolder(1), value=GetDataFolderList(), proc=lar_tools#lar_softSpikeFilterPopupProc
//	// list waves in current datafolder in list box
//	ListBox filterList, win=softSpikeFilterPanel, pos={8,76}, size={220,110}, proc=lar_tools#lar_softSpikeFilterListProc
//	ListBox filterList, listWave=filterList, selWave=filterListSel, mode=4
//	// entry for save-as name mask
//	SetVariable saveAsSV, win=softSpikeFilterPanel, pos={10,200}, size={218,16}, title="Save name"
//	SetVariable saveAsSV,value=gSaveAs, proc=lar_tools#lar_softSpikeFilterSetVProc
//	// submit button
//	Button submit,pos={173,239},size={55,20},title="Filter",fStyle=1, proc=lar_tools#lar_softSpikeFilterBtnProc
//	// extra controls in a group box
//	GroupBox extraGrpBox,pos={4,257},size={227,127},title="Extra Control",frame=0
//	// filter window size
//	SetVariable winSizeSV,pos={11,276},size={140,16},proc=lar_tools#lar_softSpikeFilterSetVProc,title="Window size (sec)"
//	SetVariable winSizeSV,limits={0,3600,60},value= gWinSize,live= 1
//	// align to whole interval checkbox
//	CheckBox winAlignCB,pos={121,296},size={16,14},proc=lar_tools#lar_softSpikeFilterCheckProc,title=""
//	CheckBox winAlignCB,variable= root:Packages:lar_tools:softSpikeFilter:winAlign
//	// base multiplier
//	SetVariable multBaseSV,pos={21,315},size={130,16},proc=lar_tools#lar_softSpikeFilterSetVProc,title="Multiplier = sdev*"
//	SetVariable multBaseSV,limits={0.5,10,0.1},value=gMultBase,live= 1
//	// multiplier increment
//	SetVariable multIncrSV,pos={12,337},size={140,16},proc=lar_tools#lar_softSpikeFilterSetVProc,title="Multiplier increment"
//	SetVariable multIncrSV,limits={-5,5,0.1},value= gMultIncr,live= 1
//	// # of interations
//	SetVariable multPassSV,pos={38,359},size={115,16},proc=lar_tools#lar_softSpikeFilterSetVProc,title="# of iterations"
//	SetVariable multPassSV,limits={1,10,1},value= gMultPass,live= 1
//	
//	PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList()))
//	lar_populateLB(filterList, filterListSel, 1)
//End
//// contains preferences information for softSpikeFilter panel
//Structure lar_softSpikeFilterPrefs
//	uint32 		version 			// YYYYMMDD date structure last modified
//	STRUCT 	Rect panel 		// left, top, right, bottom
//	char 		saveAs[31]		// default mask for naming waves
//	uint16 		winSize			// default window size
//	char 		winAlign			// default align window time values to whole intervals
//	float 		multBase 		// default base multiplier
//	float 		multIncr			// default multiplier increment
//	char 		multPass		// default number of passes
//EndStructure
//// loads/creates preference information for softSpikeFilter
//Static Function lar_softSpikeFilterLoadPrefs(prefs, [reset])
//	STRUCT lar_softSpikeFilterPrefs &prefs
//	variable reset							// optional: non-zero to reset prefs to default
//	variable currentVersion = 20100917		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_softSpikeFilter, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.panel.left = 162						// dot notion to access substructure
//		prefs.panel.top = 184
//		prefs.panel.right = 398
//		prefs.panel.bottom = 574
//		prefs.saveAs = "*"						// new name * = old name
//		prefs.winSize = 900						// 15 min window
//		prefs.winAlign = 1							// do align to whole intervals
//		prefs.multBase = 3.6
//		prefs.multIncr = 0.3
//		prefs.multPass = 3
//		
//		lar_softSpikeFilterSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves current preference information for softSpikeFilter
//Static Function lar_softSpikeFilterSavePrefs(prefs)
//	STRUCT lar_softSpikeFilterPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_softSpikeFilter, prefs
//	return (V_flag)
//End
//// reloads default preference information for softSpikeFilterPanel
//Static Function lar_softSpikeFilterResetPrefs()
//	STRUCT lar_softSpikeFilterPrefs prefs
//	// lar_print(1, "Panel preferences reset for soft spike filter")
//	return lar_softSpikeFilterLoadPrefs(prefs, reset=1)
//End
////
//Static Function lar_softSpikeFilterPanelHook(infostr)
//	string infostr
//	STRUCT lar_softSpikeFilterPrefs prefs			// invoke function preferences
//	string event = StringByKey("EVENT",infoStr)		// see what happened to the window
//	strswitch (event)								// depending on string value
//		case "activate": 								// brought into focus
//			PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList()))
//			wave/T/Z filterList = $lar_getGlobalWRef("filterList", "softSpikeFilter", 0) // refer to wave list
//			wave/Z filterListSel = $lar_getGlobalWRef("filterListSel", "softSpikeFilter", 80) // refer to selection list
//			lar_populateLB(filterList, filterListSel, 1) 		// refresh wave list box
//			break										// do nothing
//		case "deactivate":							// left focus
//			break										// do nothing
//		case "kill":									// about to be killed unless return(2)
//			break
//		case "moved":								// if moved
//		case "resize":								// or resized
//			lar_softSpikeFilterLoadPrefs(prefs)				// look up current prefs
//			GetWindow softSpikeFilterPanel wsize			// grab new position info
//			variable scale = ScreenResolution / 72			// convert from points to device coords
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			lar_softSpikeFilterSavePrefs(prefs)				// save prefs
//			break
//		default:
//			// do nothing
//	endswitch
//	return 0
//End
//// handles all button actions for softSpikeFilterPanel
//Static Function lar_softSpikeFilterBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			strswitch (ba.ctrlName) 					// and who sent it
//				case "submit":							//  submit button
//					ControlInfo/W=$ba.win inFolder				// get info from folder popup
//					string location = S_value					// save to local variable
//					ControlInfo/W=$ba.win saveAsSV 			// get info from saveas setvariable
//					SVAR saveAs = $(S_datafolder+S_value) 	// refer to global string its tied to
//					ControlInfo/W=$ba.win winSizeSV 			// get info from window size SV
//					variable winSize = V_value					// save locally
//					ControlInfo/W=$ba.win winAlignCB			// get info from window align checkbox
//					variable winAlign = V_value 				// save locally
//					ControlInfo/W=$ba.win multBaseSV 		// get info from base multiplier SV
//					variable multBase = V_value				// save locally
//					ControlInfo/W=$ba.win multIncrSV 			// get info from multiplier increment SV
//					variable multIncr = V_value					// save locally
//					ControlInfo/W=$ba.win multPassSV			// get info from # of passes SV
//					variable multPass = V_value				// save locally
//		
//					wave/T filterList = $lar_getGlobalWRef("filterList", "softSpikeFilter", 0) // refer to wave list
//					wave filterListSel = $lar_getGlobalWRef("filterListSel", "softSpikeFilter", 80) // refer to selection list
//					if ( sum(filterListSel) == 0 )					// if no waves selected
//						return 1									// return failure
//					else
//						Make/WAVE/N=(sum(filterListSel)) wavesList
//						variable i, j=0								// counters
//						for (i=0; i<numpnts(filterList); i+=1) 			// for each wave in list
//							if ( filterListSel[i] & 0x01 )					// if is selected
//								wavesList[j] = $filterList[i] 					// add ref to list
//								j+=1 									// advance output indice
//							endif
//						endfor
//						lar_softSpikeFilter(wavesList,saveAs=saveAs,winSize=winSize,winAlign=winAlign,multBase=multBase,multIncr=multIncr,multPass=multPass)
//					endif
//				break
//			endswitch
//		case -1:									// control being killed
//		case 1: 									// mouse down
//		case 3:									// mouse up outside control
//		case 4:									// mouse moved on control
//		case 5:									// mouse enter control
//		case 6:									// mouse leave control
//			break
//	endswitch								// end event code switch
//	return 0									// return success
//End
//// handles all checkboxes on softSpikeFilterPanel
//Static Function lar_softSpikeFilterCheckProc(ca) : CheckBoxControl
//	STRUCT WMCheckboxAction &ca		// checkbox structure passed in
//	switch (ca.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT lar_softSpikeFilterPrefs prefs	// create prefs structure
//			lar_softSpikeFilterLoadPrefs(prefs)			// populate prefs
//			strswitch (ca.ctrlName)				// based on which checkbox
//				case "winAlignCB":					// whether to align to intervals
//					NVAR gWinAlign = $lar_getGlobalVRef("winAlign", "softSpikeFilter", 1)
//					prefs.winAlign = gWinAlign			// grab global save to prefs
//					break
//			endswitch
//			lar_softSpikeFilterSavePrefs(prefs)		// store prefs
//			break
//		case -1:								// control being killed
//			break
//	endswitch
//	return 0
//End
//// handles all drop-down menus on softSpikeFilterPanel
//Static Function lar_softSpikeFilterPopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa					// drop-down structure is passed in
//	switch (pa.eventCode)							// depending on event code
//		case 2: 										// mouse up
//			STRUCT lar_softSpikeFilterPrefs prefs				// create prefs structure
//			lar_softSpikeFilterLoadPrefs(prefs)						// invoke current settings
//			strswitch (pa.ctrlName)						// switch on which drop-down changed
//				case "inFolder":								//  changing working directory
//					if ( DataFolderExists(pa.popStr) )				// if directory exists
//						SetDataFolder $pa.popStr						// switch to
//					else											// otherwise
//						// look up index # of current datafolder in directory list used by panels
//						variable itemNum = 1+WhichListItem(GetDataFolder(1), GetDataFolderList())
//						// reset the popup of the softSpikeFilterPanel to current datafolder
//						PopupMenu inFolder, win=softSpikeFilterPanel, mode=(itemNum)
//					endif
//					wave/T/Z filterList = $lar_getGlobalWRef("filterList", "softSpikeFilter", 0) // refer to wave list
//					wave/Z filterListSel = $lar_getGlobalWRef("filterListSel", "softSpikeFilter", 80) // refer to selection list
//					lar_populateLB(filterList, filterListSel, 1)	// refresh listbox of waves in folder
//					break
//			endswitch
//			lar_softSpikeFilterSavePrefs(prefs)				// save preference changes
//			break
//		case -1: 										// control being killed
//			break
//	endswitch									// end event code switch
//	return 0										// return success value
//End											// end pop-up handler
//// handles all variable input boxes on softSpikeFilterPanel 
//Static Function lar_softSpikeFilterSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv				// setvariable structure passed in
//	switch (sv.eventCode)							// switch based on what happened
//		case 1:										// mouse up
//		case 2: 										// enter key
//		case 3:										// live update
//			STRUCT lar_softSpikeFilterPrefs prefs			// create prefs structure
//			lar_softSpikeFilterLoadPrefs(prefs)				// populate prefs
//			strswitch (sv.ctrlName)						// depending on which control sent
//				case "saveAsSV":							// wave save-as mask
//					SVAR gSaveAs = $lar_getGlobalSRef("saveAs", "softSpikeFilter", prefs.saveAs) // refer to global
//					if ( strsearch(gSaveAs, "*", 0) == -1 )			// if no asterisk in new mask
//						DoAlert 1, "Alert: the save name has no wildcard (*) and will be the same regardless of how many waves are processed. Is this OK?"
//						if ( V_flag == 2 )								// if clicked 'no'
//							gSaveAs = prefs.saveAs 						// reset to last value (via prefs)
//						endif
//					endif 
//					gSaveAs = cleanupMask( gSaveAs )			// conform to strict naming
//					prefs.saveAs = gSaveAs						// save to prefs
//					break
//				case "winSizeSV":							// changed window size
//					NVAR gWinSize = $lar_getGlobalVRef("winSize", "softSpikeFilter", prefs.winSize) // access global
//					gWinSize = trunc(gWinSize) 					// remove fractions
//					prefs.winSize = gWinSize						// save to prefs
//					break
//				case "multBaseSV": 							// changed base multiplier
//					NVAR gMultBase = $lar_getGlobalVRef("multBase", "softSpikeFilter", prefs.multBase) // access global
//					prefs.multBase = gMultBase					// save to prefs
//					break
//				case "multIncrSV":							// changed multiplier increment
//					NVAR gMultIncr = $lar_getGlobalVRef("multIncr", "softSpikeFilter", prefs.multIncr) // access global
//					prefs.multIncr = gMultIncr						// save to prefs
//					break
//				case "multPassSV": 							// changed # of passes
//					NVAR gMultPass = $lar_getGlobalVRef("multPass", "softSpikeFilter", prefs.multPass) // access global
//					gMultPass = trunc(gMultPass)					// keep integer
//					prefs.multPass = gMultPass 					// save to prefs
//			endswitch
//			lar_softSpikeFilterSavePrefs(prefs)				// store prefs
//			break
//		case -1:										// control being killed
//			break
//	endswitch
//	return 0
//End


//// lar_nanFill( waves, mode [, num1, cmp1excl, num2, cmp2excl, lowP, highP] )
////
//// parameters
////	waves			wave of references to target waves
////	mode			designates how comparisons are made
////		0				all points range (lowP) to (highP) are set to NAN 
////		1				equal to (num1)
////		2				greater than (num1)
////		3				less than (num1)
////		4				between (num1) and (num2)
////		5				outside (num1) and (num2)
////		6				within (num1) of (num2)
////					An error (-1) is returned if a mode is selected but not supplied with appropriate (num#) arguments
////
//// optional parameters
////	lowP			lowest index to affect (inclusive) 	(default: 0; returns error if omitted for mode 0)
////	highP			highest index to affect (inclusive) 	(default: inf; returns error if omitted for mode 0)
////	num1			comparison number 1 				(returns error if needed & omitted)
////	cmp1excl		non-zero=exclusive comparison to (num1); 0=inclusive (default)
////	num2			comparison number 2 				(returns error if needed & omitted)
////	cmp2excl		non-zero=exclusive comparison to (num2); 0=inclusive (default)
////
//// returns 0 for success
//// 
//// For each wave provided, function sets values equal to NAN depending on mode specified. If mode=0, all points in
//// the range specified by lowP & highP (inclusive) are set NAN; omitting index boundaries causes function to abort.
//// If mode=1,2,3,4,5 or 6, points are set NAN only if the comparison operator returns true; omitting number arguments
//// required by these modes causes function to abort while omitted index arguments default to include the whole wave.
//// Optional parameters cmp1excl & cmp2excl control whether comparisons in modes 1-6 are done inclusively or
//// exclusively: a non-zero value changes to exclusive comparison
//Function lar_nanFill( waves, mode, [lowP, highP, num1, cmp1excl, num2, cmp2excl] )
//	wave/WAVE waves							// wave of references to target waves
//	variable mode								// numeric type-code of comparison to make
//	variable lowP									// lower indice boundary
//	variable highP								// higher indice boundary
//	variable num1								// comparison # 1
//	variable cmp1excl								// boolean: true=exclusive comparison (default: false)
//	variable num2								// comparison # 2
//	variable cmp2excl								// boolean: true=exclusive comparison (default: false)
//	
//	variable i, j									// counters
//	variable lo, hi									// working index values
//	variable numNans								// working # of points removed
//	string name									// working string to create names
//	
//	name = "Default settings loaded: "				// create msg to add onto
//	switch (mode)								// check: proper variables for this mode?
//		case 0:										// set all points in range 
//			If ( ParamIsDefault(lowP) || ParamIsDefault(highP) ) // if no range was specified
//				// lar_print(0, "lar_nanFill() is missing range specifiers. Aborted") // error msg
//				return -1										// return failure
//			elseif ( (numtype(lowP)!=0) || (numtype(highP)==2) ) // if indices were non-numeric
//				// lar_print(0, "lar_nanFill() received out-of-range index values") // error msg
//				return -1										// return failure
//			endif
//			break										// stop this case
//		case 4:										// between
//		case 5:										// outside
//		case 6:										// within
//			if ( ParamIsDefault(num2) ) 					// if comparison number 2 not specified
//				// lar_print(0, "lar_nanFill() is missing comparison value #2. Aborted") // error msg
//				return -1										// return failure
//			elseif ( numtype(num2) != 0 )					// if not a normal number (inf or nan)
//				// lar_print(0, "lar_nanFill() received non-numeric value in #2. Aborted") // error msg
//				return -1
//			endif
//		case 1:										// equal to 
//		case 2:										// greater than
//		case 3:										// less than
//			if ( ParamIsDefault(num1) )						// if comparison number not specified
//				// lar_print(0, "lar_nanFill() is missing comparison value #1. Aborted") // error msg
//				return -1										// return failure
//			elseif ( numtype(num1) != 0 )					// if not normal number
//				// lar_print(0, "lar_nanFill() received non-numeric value in #1. Aborted") // error msg
//				return -1										// return failure
//			endif
//			if ( ParamIsDefault(lowP) )						// if lower indice not specified
//				lowP = 0										// set default
//				name += "low index boundary (0); "				// add to msg
//			endif
//			if ( ParamIsDefault(highP) )						// if higher indice not specified
//				highP = inf									// set default
//				name += "high index boundary (+Inf); "			// add to msg
//			endif
//			if ( ParamIsDefault(cmp1excl) )					// if incl/excl not specified
//				cmp1excl = 0								// set default (inclusive)
//				name += "comparing #1 inclusive; "				// add to msg
//			endif
//			if ( ParamIsDefault(cmp2excl) )					// if incl/excl not specified
//				cmp2excl = 0								// set default (inclusive)
//				name += "comparing #2 inclusive; "				// add to msg
//			endif
//			break										// end this case
//	endswitch
//	// lar_print(1, "Starting lar_nanFill(...) at "+time())				// print start-up message
//	if ( strlen(name) > 30 )							// if appended on default settings msgs
//		// lar_print(2, name)								// print default settings msg
//	endif
//	
//	if ( lowP > highP )								// if index #s are backwards
//		i = highP									// store a var to temp
//		highP = lowP									// swap values
//		lowP = i										// restore saved value
//		// lar_print(2, "Index parameters lowP & highP were provided in reverse order (lowP > highP)... swapped indices")		
//	endif
//	if ( (mode==4 || mode==5) && (num1>num2) )	// if comparison numbers are backwards
//		i = num2										// store one to temp var
//		num2 = num1								// swap value over saved one
//		num1 = i										// restore saved value
//		// lar_print(2, "Comparison parameters num1 & num2 were provided in reverse order (num1 > num2).... swapped values")		
//	endif
//	if ( (mode==6) && (num1<0) )					// if comparing to within certain value
//		num1 = abs(num1) 							// ensure difference is positive
//		// lar_print(2, "Provided tolerance value was negative... using absolute value instead")
//	endif
//	
//	for (i=0; i<numpnts(waves); i+=1)				// for each wave in list
//		wave/Z src = waves[i]						// grab reference from list
//		if ( !WaveExists(src) )	 					// if missing 
//			// lar_print(0,"Wave "+NameOfWave(src)+" was missing. Skipping...")
//			continue									// skip rest of this for loop
//		elseif ( WaveDims(src) != 1 )				// or if multidimensional
//			// lar_print(0, "Wave "+NameOfWave(src)+" is multidimensional. Skipping...")
//			continue									// skip remainder of loop
//		elseif ( WaveType(src, 1)==2 && mode != 0)	// or if text wave and not a fill situation
//			// lar_print(0, "Wave "+NameOfWave(src)+" is text - comparisons are not valid. Skipping...")
//			continue
//		endif
//		name = NameOfWave(src)						// get wave's name
//		
//		lo = (lowP < 0) ? 0 : lowP						// reset lowP to zero if negative 
//		hi = (highP >= numpnts(src)) ? (numpnts(src)-1) : highP // reset highP to last index if out of range
//		numNans = 0								// reset counter
//		
//		if ( (mode==0) && (lo==0) && (hi==(numpnts(src)-1)) ) // if filling all points & range is entire wave
//			DoAlert 1, "Warning: wave "+name+" is to be filled entirely with NANs! Select 'Yes' to continue, 'No' to skip." // ask
//			If ( V_flag == 2 )							// if clicked 'no'
//				continue									// move on to next wave in list
//			endif
//		endif				
//		
//		switch (mode)								// depending on comparison operator
//			case 0:										// all points in specified range
//				src[lo,hi] = NAN								// set whole block to NAN
//				break
//			case 1: 										// equal to
//				src = ((p>=lo)&&(p<=hi)&&(src[p]==num1)) ? NAN : src[p] // assign NANs when ((...)) is true
//				break
//			case 2: 										// greater than
//				if ( cmp1excl )								// if comparing exclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>num1)) ? NAN : src[p] // assign NANs when ((...)) is true
//				else 										// if comparing inclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>=num1)) ? NAN : src[p] // assign NANs where ((...)) is true
//				endif
//				break
//			case 3: 										// less than
//				if ( cmp1excl )								// if comparing exclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]<num1)) ? NAN : src[p] // assign NANs when ((...)) is true
//				else 										// if comparing inclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]<=num1)) ? NAN : src[p] // assign NANs where ((...)) is true
//				endif
//				break
//			case 4: 										// between
//				if ( cmp1excl && cmp2excl )					// if both compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>num1)&&(src[p]<num2)) ? NAN : src[p] // assign NANs when ((...)) = true
//				elseif ( cmp1excl && !cmp2excl )				// if only num1 compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>num1)&&(src[p]<=num2)) ? NAN : src[p] // assign NANs when ((...)) = true
//				elseif ( !cmp1excl && cmp2excl )				// if only num2 compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>=num1)&&(src[p]<num2)) ? NAN : src[p] // assign NANs when ((...)) = true
//				else											// only combo left is both compared inclusively
//					src = ((p>=lo)&&(p<=hi)&&(src[p]>=num1)&&(src[p]<=num2)) ? NAN : src[p] // assign NANs when ((...)) = true
//				endif
//				break
//			case 5: 										// outside
//				if ( cmp1excl && cmp2excl )					// if both compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&((src[p]<num1)||(src[p]>num2))) ? NAN : src[p] // assign NANs when ((...)) = true
//				elseif ( cmp1excl && !cmp2excl )				// if only num1 compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&((src[p]<num1)||(src[p]>=num2))) ? NAN : src[p] // assign NANs when ((...)) = true
//				elseif ( !cmp1excl && cmp2excl )				// if only num2 compared exclusively
//					src = ((p>=lo)&&(p<=hi)&&((src[p]<=num1)||(src[p]>num2))) ? NAN : src[p] // assign NANs when ((...)) = true
//				else											// only combo left is both compared inclusively
//					src = ((p>=lo)&&(p<=hi)&&((src[p]<=num1)||(src[p]>=num2))) ? NAN : src[p] // assign NANs when ((...)) = true
//				endif				
//				break
//			case 6: 										// within
//				if ( cmp1excl )								// if comparing num1 (difference value) exclusively
//					src = ((p>=lo)&&(p<=hi)&&(abs(src[p]-num2)<num1)) ? NAN : src[p] // assign NANs when ((...)) = true
//				else											// if comparing inclusively
//					src = ((p>=lo)&&(p<=hi)&&(abs(src[p]-num2)<=num1)) ? NAN : src[p] // assign NANs when ((...)) = true
//				endif
//				break
//		endswitch
//	endfor
//	
//	// lar_print(1, "Finished filling NANs at "+time()) 		// msg
//	return 0 											// signal success
//End
//// builds and displays nanFillPanel
//Function lar_nanFillPanel() : Panel					// show in panel menu
//	DoWindow/F nanFillPanel							// attempt to bring to focus
//	if (V_flag != 0)											// if exists
//		return 0													// leave function
//	endif
//
//	STRUCT lar_nanFillPrefs prefs						// invoke preferences variable
//	lar_nanFillLoadPrefs(prefs)								// look up prefs
//	variable left = prefs.panel.left								// use shorter local names
//	variable top = prefs.panel.top
//	variable right = prefs.panel.right
//	variable bottom = prefs.panel.bottom
//
//	// access/create globals for SetVariable
//	NVAR gLowP = $lar_getGlobalVRef("lowP", "nanFill", prefs.lowP)
//	NVAR gHighP = $lar_getGlobalVRef("highP", "nanFill", prefs.highP)
//	NVAR gNum1 = $lar_getGlobalVRef("num1", "nanFill", prefs.num1)
//	NVAR gCmp1excl = $lar_getGlobalVRef("cmp1excl", "nanFill", prefs.cmp1excl)
//	NVAR gNum2 = $lar_getGlobalVRef("num2", "nanFill", prefs.num2)
//	NVAR gCmp2excl = $lar_getGlobalVRef("cmp2excl", "nanFill", prefs.cmp2excl)
//	wave/T nanList = $lar_getGlobalWRef("nanList", "nanFill", 0)
//	wave nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80)
//
//	NewPanel/W=(left,top,right,bottom)/K=1/N=nanFillPanel as "NAN Fill Tools"
//	SetWindow $S_name, hook(default)=lar_nanFillPanelHook
//	SetDrawLayer UserBack
//	SetDrawEnv fsize= 14 
//	DrawText 16,22,"NAN Fill"
//	DrawText 11,187,"Index range to affect (inclusive):"
//	DrawText 9,236,"Set values to NAN when:"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 96, 281,"Do comparison"
//	PopupMenu inFolder pos={4,25},size={250,21},bodyWidth=250,proc=lar_nanFillPopupProc
//	PopupMenu inFolder mode=1,popvalue="root:",value= #"GetDataFolderList()",popvalue=GetDataFolder(1)
//	ListBox wavesLB pos={4,54},size={250,112},mode=4,listWave=nanList,selWave=nanListSel
//	SetVariable lowpSV pos={14,190},size={100,16},bodyWidth=100,proc=lar_nanFillSetVProc,title=" "
//	SetVariable lowpSV limits={0,inf,0},value=gLowP,format="%d"
//	SetVariable highpSV pos={121,190},size={113,16},bodyWidth=100,proc=lar_nanFillSetVProc,title="to"
//	SetVariable highpSV limits={0,inf,0},value=gHighP,format="%d"
//	PopupMenu modePU pos={4,239},size={250,21},bodyWidth=250,proc=lar_nanFillPopupProc,mode=(prefs.mode+1)
//	PopupMenu modePU value= #"\"all points;equal to #1;greater than #1;less than #1;between #1 and #2;outside #1 and #2;within #1 of #2;\""
//	SetVariable num1SV pos={7,283},size={77,16},bodyWidth=60,proc=lar_nanFillSetVProc,title="#1",limits={-inf,inf,0},value=gNum1
//	CheckBox num1CB pos={100,284},size={66,14},title="exclusive",variable=gCmp1excl
//	SetVariable num2SV pos={7,305},size={77,16},bodyWidth=60,proc=lar_nanFillSetVProc,title="#2",limits={-inf,inf,0},value=gNum2
//	CheckBox num2CB pos={100,305},size={66,14},title="exclusive",variable=gCmp2excl
//	Button submit pos={193,291},size={50,20},proc=lar_nanFillBtnProc,title="NANs!",fStyle=1
//
//	PopupMenu inFolder mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList())) // set selection to current DF
//	lar_populateLB(nanList, nanListSel, 3) 			// fill listbox with waves in datafolder
//	if ( prefs.mode == 0 )	 								// if mode is "all points"
//		SetVariable num1SV disable=2		// disable entry of both numbers
//		Checkbox num1CB disable=2 		
//		SetVariable num2SV disable=2 	
//		Checkbox num2CB disable=2
//	elseif ( prefs.mode < 4 )								// or if not between/outside/within
//		SetVariable num2SV disable=2		// only disable num2
//		Checkbox num2CB disable=2
//	endif
//End	
//// preferences information for nanFillPanel
//Structure lar_nanFillPrefs
//	uint32	version			// YYYYMMDD
//	STRUCT Rect panel		// left, top, right, bottom
//	uchar 	mode			// numeric type-code of comparison to make
//	double 	num1			// comparison # 1
//	uchar 	cmp1excl		// boolean: true=inclusive comparison
//	double 	num2			// comparison # 2
//	uchar 	cmp2excl		// boolean: true=inclusive comparison
//	uint32 	lowP			// lower indice boundary
//	uint32 	highP			// higher indice boundary
//EndStructure
//// restores preferences to nanFillPanel
//Function lar_nanFillLoadPrefs(prefs, [reset])
//	STRUCT lar_nanFillPrefs &prefs
//	variable reset							// optional: nonzero to reset to defaults
//	variable currentVersion = 20101015		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_nanFill, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.panel.left = 390						// dot notion to access substructure
//		prefs.panel.top = 120
//		prefs.panel.right = 650
//		prefs.panel.bottom = 449
//		
//		prefs.mode = 0							// default to fill all points
//		prefs.num1 = NAN						// no default value
//		prefs.cmp1excl = 0						// default inclusive comparisons
//		prefs.num2 = NAN						// no default value
//		prefs.cmp2excl = 0						// default inclusive comparisons
//		prefs.lowP = 0							// default lowest point
//		prefs.highP = inf							// default past end of wave
//		
//		lar_nanFillSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves preferences for function
//Function lar_nanFillSavePrefs(prefs)
//	STRUCT lar_nanFillPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_nanFill, prefs
//	return (V_flag)
//End
//// resets preferences to default values
//Function lar_nanFillResetPrefs()
//	STRUCT lar_nanFillPrefs prefs
//	// lar_print(1, "Panel preferences reset for Hard Filter/NAN Fill")
//	return lar_nanFillLoadPrefs(prefs, reset=1)
//End
//// handles preferences concerning panel appearance, position, etc
//Function lar_nanFillPanelHook(s)
//	STRUCT WMWinHookStruct &s	
//	STRUCT lar_nanFillPrefs prefs				// invoke function preferences
//	switch (s.eventCode)								// depending on string value
//		case 0: 		// activate
//			PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList())) // set popup to current DF
//			wave/T/Z nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//			wave/Z nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//			lar_populateLB(nanList, nanListSel, 3)		// refresh listbox of waves in datafolder
//		case 12: 	// moved
//		case 6: 		// resize
//			lar_nanFillLoadPrefs(prefs)				// look up current prefs
//			GetWindow $s.winName wsize			// grab new position info
//			variable scale = ScreenResolution / 72 		// convert points to device units
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			lar_nanFillSavePrefs(prefs)					// save prefs
//			break
//		case 1: 		// deactivate
//		case 2: 		// kill
//			break
//	endswitch
//	return 0
//End
//// handles all buttons on nanFillPanel
//Function lar_nanFillBtnProc(ba)  : ButtonControl   
//	STRUCT WMButtonAction &ba				// button structure passed in
//	switch( ba.eventCode )					// depending on event code
//		case 2: 									// mouse up
//			strswitch (ba.ctrlName)					// depending on which control called
//				case "submit":							//  submit button
//					ControlInfo/W=$ba.win inFolder				// get info from folder popup
//					string location = S_value					// save to local variable
//					ControlInfo/W=$ba.win lowpSV				// get info from lower index SV
//					variable lowP = V_value					// save locally
//					ControlInfo/W=$ba.win highpSV			// get info from higher index SV
//					variable highP = V_value					// save locally
//					ControlInfo/W=$ba.win modePU			// get info from mode drop-down box
//					variable mode = (V_value - 1)				// save locally & adjust to be zero-indexed
//					ControlInfo/W=$ba.win num1SV			// get info from num1 SetVariable
//					variable num1 = V_value					// save locally
//					ControlInfo/W=$ba.win num1CB			// get info from incl/excl checkbox
//					variable cmp1excl = V_value				// save locally
//					ControlInfo/W=$ba.win num2SV 			// get info from num2 SetVariable
//					variable num2 = V_value					// save locally
//					ControlInfo/W=$ba.win num2CB			// get info from incl/excl checkbox
//					variable cmp2excl = V_value				// save locally
//		
//					wave/T nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//					wave nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//					if ( sum(nanListSel) == 0 )					// if no waves selected
//						// lar_print(0, "lar_nanFill(): no waves were selected")
//						return 1									// return failure
//					else
//						Make/WAVE/N=(sum(nanListSel)) wavesList
//						variable i, j=0								// counters
//						for (i=0; i<numpnts(nanList); i+=1) 			// for each wave in list
//							if ( nanListSel[i] & 0x01 )					// if is selected
//								wavesList[j] = $nanList[i] 					// add ref to list
//								j+=1 									// advance output indice
//							endif
//						endfor
//						lar_nanFill(wavesList,mode,lowP=lowP,highP=highP,num1=num1,cmp1excl=cmp1excl,num2=num2,cmp2excl=cmp2excl)
//						Killwaves/Z wavesList
//					endif
//					break								// end submit button
//				default:
//					// do nothing
//			endswitch
//			break									// end case mouse up
//		case -1:									// control being killed
//		case 1: 									// mouse down
//		case 3:									// mouse up outside control
//		case 4:									// mouse moved on control
//		case 5:									// mouse enter control
//		case 6:									// mouse leave control
//			break
//	endswitch								// end event code switch
//	return 0									// return success
//End
//// handles all check boxes on nanFillPanel
//Function lar_nanFillCheckProc(ca) : CheckBoxControl
//	STRUCT WMCheckboxAction &ca		// checkbox structure passed in
//	switch (ca.eventCode)					// what happened to it?
//		case 2:								// mouse up event
//			STRUCT lar_nanFillPrefs prefs		// create prefs structure
//			lar_nanFillLoadPrefs(prefs)			// poplate prefs
//			strswitch (ca.ctrlName)				// based on which checkbox
//				case "num1CB":						// exclusive comparison on num1?
//					NVAR gCmp1excl = $lar_getGlobalVRef("cmp1excl", "nanFill", prefs.cmp1excl) // refer to global
//					prefs.cmp1excl = gCmp1excl			// save value to prefs
//					break
//				case "num2CB": 						// exclusive comparison on num2?
//					NVAR gCmp2excl = $lar_getGlobalVRef("cmp2excl", "nanFill", prefs.cmp2excl) // refer to global
//					prefs.cmp2excl = gCmp2excl			// save value to prefs
//					break
//			endswitch
//			lar_nanFillSavePrefs(prefs)			// store prefs
//			break
//		case -1:								// control being killed
//			break
//	endswitch
//	return 0
//End
//// handles all drop-down menus on nanFillPanel
//Function lar_nanFillPopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa					// drop-down structure is passed in
//	switch (pa.eventCode)							// depending on event code
//		case 2: 										// mouse up
//			STRUCT lar_nanFillPrefs prefs				// create prefs structure
//			lar_nanFillLoadPrefs(prefs)						// invoke current settings
//			strswitch (pa.ctrlName)						// switch on which drop-down changed
//				case "inFolder":								// working directory
//					if ( DataFolderExists(pa.popStr) )				// if directory exists
//						SetDataFolder $pa.popStr						// switch to
//					else											// otherwise
//						// look up index # of current datafolder in directory list used by panels
//						variable itemNum = 1+WhichListItem(GetDataFolder(1), GetDataFolderList())
//						// reset the popup of the softSpikeFilterPanel to current datafolder
//						PopupMenu inFolder, win=$pa.win, mode=(itemNum)
//					endif
//					wave/T/Z nanList = $lar_getGlobalWRef("nanList", "nanFill", 0) // refer to wave list
//					wave/Z nanListSel = $lar_getGlobalWRef("nanListSel", "nanFill", 80) // refer to selection list
//					lar_populateLB(nanList, nanListSel, 3)		// refresh listbox of waves in folder
//					break
//				case "modePU":								// style of fill (all, comparison, etc)
//					switch (pa.popNum-1) 						// based on mode selection
//						case 0:										// all points
//							SetVariable num1SV,win=$pa.win,disable=2 // disable entry for both numbers
//							Checkbox num1CB,win=$pa.win,disable=2 		
//							SetVariable num2SV,win=$pa.win,disable=2 	
//							Checkbox num2CB,win=$pa.win,disable=2
//							break
//						case 1: 										// equal to
//						case 2: 										// greater than
//						case 3: 										// less than
//							SetVariable num1SV,win=$pa.win,disable=0 // restore entry for num1
//							Checkbox num1CB,win=$pa.win,disable=0 		
//							SetVariable num2SV,win=$pa.win,disable=2 // disable entry for num2
//							Checkbox num2CB,win=$pa.win,disable=2							
//							break
//						case 4: 										// between
//						case 5:										// outside
//						case 6:										// within
//							SetVariable num1SV,win=$pa.win,disable=0 // restore entry for both numbers
//							Checkbox num1CB,win=$pa.win,disable=0	
//							SetVariable num2SV,win=$pa.win,disable=0 	
//							Checkbox num2CB,win=$pa.win,disable=0
//							break
//					endswitch
//					prefs.mode = (pa.popNum-1)					// save to prefs
//				default:
//					// do nothing
//					break
//			endswitch
//			lar_nanFillSavePrefs(prefs)					// save preference changes
//			break
//		case -1: 										// control being killed
//			break
//	endswitch									// end event code switch
//	return 0										// return success value
//End											// end pop-up handler
//// handles all variable input boxes on nanFillPanel
//Function lar_nanFillSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv				// setvariable structure passed in
//	switch (sv.eventCode)							// switch based on what happened
//		case 1:										// mouse up
//		case 2: 										// enter key
//		case 3:										// live update
//			STRUCT lar_nanFillPrefs prefs			// create prefs structure
//			lar_nanFillLoadPrefs(prefs)				// populate prefs
//			strswitch (sv.ctrlName)					// depending on which control sent
//				case "lowpSV":							// lower index #
//					NVAR gLowP = $lar_getGlobalVRef("lowP", "nanFill", prefs.lowP) // global ref
//					prefs.lowP = gLowP						// save to prefs
//					break
//				case "highPSV":							// higher index #
//					NVAR gHighP = $lar_getGlobalVRef("highP", "nanFill", prefs.highP) // global ref
//					prefs.highP = gHighP						// save to prefs
//					break
//				case "num1SV":							// comparison # 1
//					NVAR gNum1 = $lar_getGlobalVRef("num1", "nanFill", prefs.num1) // global ref
//					prefs.num1 = gNum1						// save to prefs
//					break
//				case "num2SV":							// comparison # 2
//					NVAR gNum2 = $lar_getGlobalVRef("num2", "nanFill", prefs.num2) // global ref
//					prefs.num2 = gNum2						// save to prefs
//					break
//			endswitch
//			lar_nanFillSavePrefs(prefs)					// store prefs
//			break
//		case -1:										// control being killed
//			break
//	endswitch
//	return 0
//End

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////   WIND TOOLS ////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// lar_wswd(Ux, Uy, outputs, winSize [, winAlign, azimuth, declination, savePath, maxEmtpy] )
//	Ux			wind component along U
// 	Uy 			wind component along V
// 	outputs		text wave of output names determines which calculations are done
//		[0]			wind speed: vector average
//		[1] 			wind speed: scalar average
//		[2]			wind speed: scalar std dev
//		[3]			wind speed: persistence
//		[4] 			wind direction: vector average
//		[5] 			wind direction: scalar average
//		[6] 			wind direction: scalar std dev
//		[7] 			wind direction: est. vector std dev by Yamartino
//		[8]			wind direction: est. vector std dev by Mardia
//	winSize 		size of averaging window (seconds)
//	winAlign		boolean: non-zero to align windows to whole intervals (1:00, 1:30, etc); defaults ON
//	azimuth		offset of sonic orientation from North; value is added to results; defaults: 0
//	declination	offset of mag. north from true north; value is added to results (East=neg, West=pos); default: 0
//	savePath	relative path to store results in; default: same directory
//	maxEmpty	max permissible % of missing records; interval set NAN if fails; (0-100): default: disabled
//
// description
//Function lar_wswd(Ux, Uy, outputs, winSize, [winAlign, azimuth, declination, savePath, maxEmpty])
//	WAVE 		Ux 			// U-component of wind 
//	WAVE 		Uy			// V-component of wind 
//	WAVE/T 	outputs 		// text wave holds names, determines outputs
//								// [0]	wind speed: vector average
//								// [1] 	wind speed: scalar average
//								// [2]	wind speed: scalar std dev
//								// [3]	wind speed: persistence
//								// [4] 	wind direction: vector average
//								// [5] 	wind direction: scalar average
//								// [6] 	wind direction: scalar std dev
//								// [7] 	wind direction: est. vector std dev by Yamartino
//								// [8]	wind direction: est. vector std dev by Mardia
//	variable 		winSize 		// size (sec) of averaging period
//	variable 		winAlign		// boolean: non-zero for nice intervals (1:00, 1:30...); default ON
//	variable 		azimuth 		// offset of sonic orientation from north; default 0
//	variable 		declination	// offset of mag from true north; default 0
//	string 		savePath	// target path for results
//	variable 		maxEmpty	// max % of records missing before interval set to NAN; default: disabled
//	
//	variable DperR = 180/pi						// conversions
//	variable RperD = pi/180
//	variable Vaz = (azimuth+declination-180) - 90 	// angle of +V w.r.t true north = 
//
//	variable num1
//	variable i	,j							// counter
//
//	variable L, Ltime, Htime				// time trackers
//	variable phi, last, Sa, Ca, D, R, e 		// working vars
//	//variable Vaz = (sonicOrient-180)-90		// angle of +V w.r.t True North = (sonicOrient-180 = +Ux) - 90
//
//	// 1st quality check: all waves same length?
//	if ( (numpnts(tstamp)!=numpnts(Ux)) || (numpnts(tstamp)!=numpnts(Uy)) || (numpnts(Ux)!=numpnts(Uy)) )
//		print "Waves supplied to makeWSWD_func(...) were not of the same length"
//		abort
//	endif
//	
//	// verify data folder entry is 'good' and, if needed, switch to folder
////	string savDF = GetDataFolder(1)
////	if ( cmpstr(DFname[0],":") == 0 )								// if first char is a colon
////		DFname = DFname[1,strlen(DFname)-1]							// delete it
////	endif
////	if ( cmpstr(DFname[strlen(DFname)-1,strlen(DFname)-1 ],":")==0 )	// if last char isn't a colon
////		DFname = DFname[0,strlen(DFname)-2]							// delete it
////	endif
////	If ( strlen(DFname) > 0 )
////		NewDataFolder/O/S $DFname							// create location
////	endif
//	
//	// check time wave for discontinuities
////	for ( i=0; i<numpnts(tstamp); i+=1 )
////		if ( numtype(tstamp[i]) != 0 || tstamp[i] <= 0 )
////			doAlert 0, "Provided wave of timestamps contained empty points and could not be understood."
////			abort
////		endif
////	endfor
//	
//	num1 = numpnts(tstamp)									// # of inst. data points
//	// define working waves & refer to
//	Make/O/D/N=(num1) WSinst_tmp = NaN
//	Make/O/D/N=(num1) WDinst_tmp = NaN
//	Make/O/D/N=(num1) Dinst_tmp = NaN
//	Make/O/D/N=(num1) WDcos_tmp = NaN
//	Make/O/D/N=(num1) WDsin_tmp = NaN
//	Make/O/D/N=(num1) Ux_avg_tmp = NaN
//	Make/O/D/N=(num1) Uy_avg_tmp = NaN
//
//	// define output waves & refer to
////	If ( ParamIsDefault(tname) )
////		Make/O/D/N=(num1) startTime = NaN
////	else
////		Make/O/D/N=(num1) $tname = NaN
////		wave/D startTime = $tname
////	endif
//	if ( strlen(outputs[0]) > 0 )
//		Make/O/D/N=(num1) $outputs[0] = NaN
//		wave wsVavg = $outputs[0]
//	endif
//	if ( strlen(outputs[1]) > 0 )
//		Make/O/D/N=(num1) $outputs[1] = NaN
//		wave wsSavg = $outputs[1]
//	endif
//	if ( strlen(outputs[2]) > 0 )
//		Make/O/D/N=(num1) $outputs[2] = NaN
//		wave wsSsdev = $outputs[2]
//	endif
//	if ( strlen(outputs[3]) > 0 )
//		Make/O/D/N=(num1) $outputs[3] = NaN
//		wave wsPers = $outputs[3] 
//	endif
//	if ( strlen(outputs[4]) > 0 )
//		Make/O/D/N=(num1) $outputs[4] = NaN
//		wave wdVavg = $outputs[4]
//	endif
//	if ( strlen(outputs[5]) > 0 )
//		Make/O/D/N=(num1) $outputs[5] = NaN
//		wave wdSavg = $outputs[5]
//	endif
//	if ( strlen(outputs[6]) > 0 )
//		Make/O/D/N=(num1) $outputs[6] = NaN
//		wave wdSsdev = $outputs[6] 
//	endif
//	if ( strlen(outputs[7]) > 0 )
//		Make/O/D/N=(num1) $outputs[7] = NaN
//		wave wdYsdev = $outputs[7] 
//	endif
//	if ( strlen(outputs[8]) > 0 )
//		Make/O/D/N=(num1) $outputs[8] = NaN
//		wave wdMsdev = $outputs[8]
//	endif
//	
//	j = 0											// set output indice to 0
//	L = 0											// set average interval lower bracket indice to 0
////	Ltime = tstamp[0]									// set initial low interval time to 1st time
////	if ( evenInt )										// if desired to keep intervals "round"
////		Htime = tstamp[0] - mod(tstamp[0],86400)			// set high time to midnight, same date
////		do												// then start by
////			Htime += avgPer*60								// increasing interval length
////		while ( Htime <= Ltime )							// continue until exceeds lower time
////	else												// if "round" intervals don't matter
////		Htime = Ltime+avgPer								// set high time to low+interval
////	endif
//	for ( i=0; i<num1; i+=1 )							// for every inst. point	
//		// Wind directions computed following the 'Wind Direction Quick Reference' prepared
//		// by Gordan Maclean. Available online by search
//		//
//		// for sonic ATIs, CSAT3s, Vaz = (dir relative to True N, looking into sensors) - 90
//		// 					     = (sonicOrient - 180) - 90
//		// WS = sqrt( Usonic^2 + Vsonic^2 )
//		// WDsonic = atan2(-Usonic,-Vsonic)*DperR			w.r.t sonic orient
//		// WDmet = WDsonic + Vaz						w.r.t. true N
//		// D = atan2( avg(V), avg(U) )*DperR
//		//
//		WSinst_tmp[i] = sqrt( Ux[i]^2 + Uy[i]^2 ) 			// find inst. vector wind speed
//		D = atan2( -1*Ux[i], -1*Uy[i] )*DperR				// find inst. vector wind dir
//		WDinst_tmp[i] = LTmake0to359( D+Vaz ) 			// correct for sonic angle, adjust 0-360
//		WDcos_tmp[i] = cos(WDinst_tmp[i]*RperD) 			// track cos(..) 
//		WDsin_tmp[i] = sin(WDinst_tmp[i]*RperD)			// track sin(..)
//		
//		// Mitsuta method of computing scalar mean WD:
//		//
//		//	avg WD = mean(D) where
//		//	Di = WDi					for i=1
//		// 	Di = Di-1 + phi + 360 			for phi < -180 & i > 1
//		//	Di = Di-1 + phi 				for abs(phi) < 180 & i > 1
//		//	Di = Di-1 + phi - 360			for phi > 180 & i > 1
//		//	Di = undefined 				for phi = 180 & i > 1     ***BENDING THIS RULE***
//		//	phi = WDi - Di-1				for i > 1
//		//	WDi = azimuth angle of of wind vane for i-th sample
//		//
//		if ( i == 0 )								// for i=1
//			Dinst_tmp[i] = WDinst_tmp[i]				// set Di = WDi
//		elseif ( numtype(WDinst_tmp[i]) != 0 )		// if WDi is undefined
//			Dinst_tmp[i] = NaN						// save Di as undefined
//		elseif ( numtype(WDinst_tmp[i-1]) != 0 )		// if last WDi was undefined (assuming this one isn't)
//			Dinst_tmp[i] = WDinst_tmp[i]				// set Di = WDi as if i=1 again
//			if ( Dinst_tmp[i]-last < -180 )				// if WD is more than 180d away towards neg
//				Dinst_tmp[i] += 360						// shift 1 circle upward
//			elseif ( Dinst_tmp[i]-last > 180 )			// if WD is more than 180d away towards pos
//				Dinst_tmp[i] -= 360
//			endif
//		else
//			phi = WDinst_tmp[i] - Dinst_tmp[i-1]
//			
//			if ( phi < -180 )
//				Dinst_tmp[i] = Dinst_tmp[i-1] + phi + 360
//			elseif ( phi > 180 )
//				Dinst_tmp[i] = Dinst_tmp[i-1] + phi - 360
//			elseif ( abs(phi) <= 180 )
//				Dinst_tmp[i] = Dinst_tmp[i-1] + phi
//			endif
//			
////			if ( phi == 180 )
////				Dinst_tmp[i] = NaN
////			else
//				last = Dinst_tmp[i]
////			endif
//		endif
//		
////		If ( tstamp[i+1] >= Htime || i+1==numpnts(tstamp) )	// if next record is next interval or end of wave
////			print "interval ends @ i="+num2istr(i)+"; times are "+secs2date(Ltime,0)+" "+secs2time(Ltime,3)+" to "+secs2date(tstamp[i],0)+" "+secs2time(tstamp[i],3)
////			
////			WaveStats/Q/R=[L,i] Ux						// average Ux vector components
////			Ux_avg_tmp[j] = V_avg						// store it
////			WaveStats/Q/R=[L,i] Uy						// average Uy vector components
////			Uy_avg_tmp[j] = V_avg
////			if ( WaveExists(wsVavg) )
////				wsVavg[j] = sqrt( Ux_avg_tmp[j]^2 + Uy_avg_tmp[j]^2 )
////			endif
////			
////			WaveStats/Q/R=[L,i] WSinst_tmp 				// average inst. WS
////			if ( WaveExists(wsSavg) )						// if possible,
////				wsSavg[j] = V_avg							// save as avg. scalar WS
////			endif
////			if ( WaveExists(wsSsdev) )						// and also,
////				wsSsdev[j] = V_sdev							// scalar WS std dev
////			endif
////			If ( WaveExists(wsPers) )
////				wsPers[j] = sqrt( Ux_avg_tmp[j]^2 + Uy_avg_tmp[j]^2 ) / V_avg
////			endif
////
////			D = atan2( -1*Ux_avg_tmp[j], -1*Uy_avg_tmp[j] )*DperR
////			if ( WaveExists(wdVavg) )
////				wdVavg[j] = make0to359_tb( D+Vaz )
////			endif
////			
////			WaveStats/Q/R=[L,i] Dinst_tmp
////			If ( WaveExists(wdSavg) )
////				wdSavg[j] = make0to359_tb( V_avg )
////			endif
////			If ( WaveExists(wdSsdev) )
////				wdSsdev[j] = V_sdev
////			endif			
////
////			// Yamartino method of estimating standard deviation of wind direction via single-pass
////			// described on wikipedia, preferred by US EPA
////			// 	Sa = avg( sin(WD) ) 
////			// 	Ca = avg( cos(WD) ) 
////			//	e = sqrt(1 - (Sa^2 + Ca^2) )
////			// 	std. dev WD = arcsin(e) [1 + 0.1547e^3] *(pi/180)
////			// 
////			// Mardia estimate of std dev, based on directional statistics
////			// 	Sa = same def. as Yamartino
////			//	Ca = same def. as Yamartino
////			//	R = sqrt( Sa^2 + Ca^2 )
////			// 	std. dev. WD = sqrt( -2*ln(R) ) *(pi/180)
////			WaveStats/Q/R=[L,i] WDcos_tmp				// average cos(..)
////			Ca = V_avg
////			WaveStats/Q/R=[L,i] WDsin_tmp				// average sin(..)
////			Sa = V_avg
////			R = sqrt(Sa^2 + Ca^2)	
////			e = sqrt(1 - (Sa^2 + Ca^2)) 
////			If ( WaveExists(wdYsdev) )
////				wdYsdev[j] = asin(e)*(1+0.1547*e^3)*DperR
////			endif
////			If ( WaveExists(wdMsdev) )
////				wdMsdev[j] = sqrt( -2*ln(R) )*DperR
////			endif
////			
////			startTime[j] = Htime - avgPer*60	// store targed interval start time in output
////			
////			j += 1  						// advance output indice
////			L = i+1						// reset lower indice
////			Ltime = tstamp[L] 			// find new lower time
////			if ( evenInt )										// if desired to keep intervals "round"
////				Htime = tstamp[0] - mod(tstamp[0],86400)			// set high time to midnight, same date
////				do												// then start by
////					Htime += avgPer*60								// increasing interval length
////				while ( Htime <= Ltime )							// continue until exceeds lower time
////			else												// if "round" intervals don't matter
////				Htime = Ltime+avgPer								// set high time to low+interval
////			endif
////		endif	
////		// continue until next record in interval
//	endfor 	// i
//	
//// 	num1 -= j					// compute # of points not filled up = # to remove
////	If ( WaveExists(startTime) )
////	 	DeletePoints j, num1, startTime
////	endif
//// 	If ( WaveExists(wsVavg) )
////	 	DeletePoints j, num1, wsVavg
////	endif
////	if ( WaveExists(wsSavg) )
////		DeletePoints j, num1, wsSavg
////	endif
////	If ( WaveExists(wsSsdev) )
////		DeletePoints j, num1, wsSsdev
////	endif
////	If ( WaveExists(wsPers) )
////		DeletePoints j, num1, wsPers
////	endif
////	If ( WaveExists(wdVavg) )
////		DeletePoints j, num1, wdVavg
////	endif
////	If ( WaveExists(wdSavg) )
////		DeletePoints j, num1, wdSavg
////	endif
////	If ( WaveExists(wdSsdev) )
////		DeletePoints j, num1, wdSsdev
////	endif
////	If ( WaveExists(wdYsdev) )
////		DeletePoints j, num1, wdYsdev
////	endif
////	If ( WaveExists(wdMsdev) )
////		DeletePoints j, num1, wdMsdev
////	endif
//// 	
////	KillWaves/Z Ux_avg_tmp, Uy_avg_tmp, WSinst_tmp, WDinst_tmp, Dinst_tmp, WDcos_tmp, WDsin_tmp
////	
////	SetDataFolder $savDF		// in case moved into subfolder
//End 
// Generates horizontal wind directions and speeds using sonic wind Ux, Uy waves
//Function lar_wswdPanel() : Panel
//	DoWindow/F wswdPanel								// attempt to bring to focus
//	if (V_flag != 0)											// if successful
//		return 0													// leave function
//	endif
//
//	STRUCT lar_wswdPrefs prefs							// invoke preferences variable
//	lar_wswdLoadPrefs(prefs)								// look up prefs
//
//	// construct the basic panel
//	NewPanel/K=1/W=(prefs.panel.left,prefs.panel.top,prefs.panel.right,prefs.panel.bottom)/N=wswdPanel as "Sonic data to WS/WD"
//	SetWindow wswdPanel, hook=lar_tools#lar_wswdPanelHook
//	// add annotation
//	SetDrawLayer UserBack
//	DrawLine 273,317,290,328
//	DrawLine 291,326,273,339
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 153,504,"points set to NAN"
//	SetDrawEnv fname= "MS Sans Serif"
//	DrawText 12,519,"Set 100% to disable"
//	// working directory popup
//	PopupMenu inFolder,pos={10,9},size={250,21},bodyWidth=250,proc=lar_tools#lar_wswdPopupProc
//	PopupMenu inFolder,popvalue=GetDataFolder(1),value= #"lar_GetDataFolderList()"
//	PopupMenu inFolder,mode=(Max( (1+WhichListItem(GetDataFolder(1), GetDataFolderList())), 1 ))
//	// group the sonic components
//	GroupBox sonicGB,pos={3,38},size={262,107},title="Sonic Data",frame=0
//		// U-wave selection popup
//		PopupMenu uPU,pos={11,59},size={141,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="U"
//		PopupMenu uPU,mode=(Max( (1+WhichListItem(prefs.Uname, WaveList("*",";","TEXT:0"))), 1 ))
//		PopupMenu uPU,value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//		TitleBox Utext,pos={156,57},size={101,26},title="(+) = into sonic array,\r\t\t\tparallel to boom",frame=0
//		// V-wave selection popup
//		PopupMenu vPU,pos={12,88},size={140,21},bodyWidth=130,proc=lar_tools#lar_wswdPopupProc,title="V"
//		PopupMenu vPU,mode=(Max( (1+WhichListItem(prefs.Vname, WaveList("*",";","TEXT:0"))), 1 ))
//		PopupMenu vPU,value= #"WaveList(\"*\",\";\",\"TEXT:0\")"
//		TitleBox Vtext,pos={159,90},size={93,13},title="right-handed w.r.t U",frame=0
//		// sonic azimuth set variable
//		SetVariable azimuthSV,pos={10,119},size={91,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Azimuth"
//		SetVariable azimuthSV,format="%u",limits={0,359,1},value= _NUM:prefs.azimuth,live= 1
//		// location declination set variable
//		SetVariable declinationSV,pos={128,120},size={107,16},bodyWidth=50,proc=lar_tools#lar_wswdSetVProc,title="Declination"
//		SetVariable declinationSV,format="%.1f",limits={-180,180,0.1},value= _NUM:prefs.declination,live= 1
//	// group wind speed outputs
//	GroupBox wsGB,pos={5,152},size={260,107},title="Wind Speed Ouput Names",frame=0
//		// vector average
//		CheckBox wsVavgCB,pos={13,174},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wsVavg.checked
//		SetVariable wsVavgSV,pos={36,174},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wsVavgSV,value= _STR:prefs.wsVavg.strVal,title="Vector Average:"
//		TitleBox Utext5,pos={271,164},size={131,24},title="= (|U|\\Bmean\\M\\S2\\M + |V|\\Bmean\\M\\S2\\M)\\S1/2\\M"
//		TitleBox Utext5,frame=0
//		// scalar average
//		CheckBox wsSavgCB,pos={13,195},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wsSavg.checked
//		SetVariable wsSavgSV,pos={37,195},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wsSavgSV,value= _STR:prefs.wsSavg.strVal,title="Scalar Average:"
//		TitleBox Utext7,pos={272,190},size={117,20},title="= mean[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//		TitleBox Utext7,frame=0
//		// scalar standard deviation
//		CheckBox wsSsdevCB,pos={13,215},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wsSsdev.checked
//		SetVariable wsSsdevSV,pos={38,215},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wsSsdevSV,value= _STR:prefs.wsSsdev.strVal,title="Scalar Std Dev:"
//		TitleBox Utext8,pos={272,212},size={117,20},title="= stdev[ (|U|\\S2\\M + |V|\\S2\\M)\\S1/2\\M ]"
//		TitleBox Utext8,frame=0
//		// persistence
//		CheckBox wsPersCB,pos={13,234},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wsPers.checked
//		SetVariable wsPersSV,pos={55,234},size={202,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wsPersSV,value= _STR:prefs.wsPers.strVal,title="Persistence:"
//		TitleBox Utext6,pos={273,238},size={118,17},title="= |WS|\\Bvector\\M / WS\\Bscalar\\M"
//		TitleBox Utext6,frame=0
//	// group wind direction outputs
//	GroupBox wdGB,pos={3,268},size={262,127},title="Wind Direction Output Names",frame=0
//		// vector average
//		CheckBox wdVavgCB,pos={13,290},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wdVavg.checked
//		SetVariable wdVavgSV,pos={35,290},size={221,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wdVavgSV,value= _STR:prefs.wdVavg.strVal,title="Vector Average:"
//		TitleBox Utext3,pos={274,287},size={137,17},title="= atan2(-|Ux|\\Bmean\\M,-|Uy|\\Bmean\\M)"
//		TitleBox Utext3,frame=0
//		// scalar average
//		CheckBox wdSavgCB,pos={13,310},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wdSavg.checked
//		SetVariable wdSavgSV,pos={36,310},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wdSavgSV,value= _STR:prefs.wdSavg.strVal,title="Scalar Average:"
//		// scalar standard deviation
//		CheckBox wdSsdevCB,pos={13,330},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wdSsdev.checked
//		SetVariable wdSsdevSV,pos={38,330},size={219,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wdSsdevSV,value= _STR:prefs.wdSsdev.strVal,title="Scalar Std Dev:"
//		TitleBox Utext4,pos={294,321},size={72,13},title="Mitsuta method",frame=0
//		// vector standard deviation by Yamartino
//		CheckBox wdVsdevYCB,pos={14,351},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wdVsdevY.checked
//		SetVariable wdVsdevYSV,pos={37,350},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wdVsdevYSV,value= _STR:prefs.wdVsdevY.strVal,title="Vector Std Dev:"
//		TitleBox Utext9,pos={274,351},size={89,13},title="Yamartino estimate",frame=0
//		// vector standard deviation by Mardia
//		CheckBox wdVsdevMCB,pos={14,370},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.wdVsdevM.checked
//		SetVariable wdVsdevMSV,pos={37,370},size={220,16},bodyWidth=140,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wdVsdevMSV,value= _STR:prefs.wdVsdevM.strVal,title="Vector Std Dev:"
//		TitleBox Utext0,pos={274,373},size={74,13},title="Mardia estimate",frame=0
//	// group output options
//	GroupBox outputGB,pos={5,402},size={260,123},title="Output Options"
//		// set the wildcard value for names
//		SetVariable wildcardSV,pos={11,422},size={243,16},bodyWidth=80,proc=lar_tools#lar_wswdSetVProc
//		SetVariable wildcardSV,value= _STR:prefs.wildcardStr,title="Replace * wildcard in names with "
//		// set the averaging period or window size
//		SetVariable winsizeSV,pos={12,444},size={150,16},bodyWidth=60,proc=lar_tools#lar_wswdSetVProc
//		SetVariable winsizeSV,limits={1,86400,60},value= _NUM:prefs.winSize.numVal,live= 1,title="Window size (sec)"
//		// nice intervals checkbox
//		CheckBox winsizeCB,pos={172,445},size={82,14},proc=lar_tools#lar_wswdChkProc,title="Nice intervals",value=prefs.winSize.checked
//		// save in checkbox & set variable
//		CheckBox saveinCB,pos={12,468},size={16,14},proc=lar_tools#lar_wswdChkProc,title="",value=prefs.saveIn.checked
//		SetVariable saveinSV,pos={34,466},size={221,16},bodyWidth=181,proc=lar_tools#lar_wswdSetVProc
//		SetVariable saveinSV,value= _STR:prefs.saveIn.strVal,title="Save in"
//		// max empty points set variable
//		SetVariable maxemptySV,pos={10,489},size={139,16},bodyWidth=40,proc=lar_tools#lar_boxMeanSetVProc
//		SetVariable maxemptySV,format="%g%",limits={0,100,0},value= _NUM:prefs.maxEmpty,live= 1,title=" Intervals missing >="
//	// hide/show formulas button
//	Button formulaB,pos={19,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="To fix"
//	// submit button
//	Button submitB,pos={158,532},size={100,20},proc=lar_tools#lar_wswdBtnProc,title="Make WS/WD",fStyle=1
//	
//	// disable any setvariable entries without a checkmark nearby
//	SetVariable wsVavgSV,disable= (prefs.wsVavg.checked ? 0 : 2)
//	SetVariable wsSavgSV,disable= (prefs.wsSavg.checked ? 0 : 2)
//	SetVariable wsSsdevSV,disable= (prefs.wsSsdev.checked ? 0 : 2)
//	SetVariable wsPersSV,disable= (prefs.wsPers.checked ? 0 : 2)
//	SetVariable wdVavgSV,disable= (prefs.wdVavg.checked ? 0 : 2)
//	SetVariable wdSavgSV,disable= (prefs.wdSavg.checked ? 0 : 2)
//	SetVariable wdSsdevSV,disable= (prefs.wdSsdev.checked ? 0 : 2)
//	SetVariable wdVsdevYSV,disable= (prefs.wdVsdevY.checked ? 0 : 2)
//	SetVariable wdVsdevMSV,disable= (prefs.wdVsdevM.checked ? 0 : 2)
//	SetVariable saveinSV,disable= (prefs.saveIn.checked ? 0 : 2)
//	
//End
//// preferences information for windStatsPanel
//Structure lar_wswdPrefs
//	uint32			version		// YYYYMMDD
//	STRUCT Rect 	panel		// left, top, right, bottom
//	uchar 			Uname[MAX_OBJ_NAME] // name of U wave
//	uchar 			Vname[MAX_OBJ_NAME] // name of V wave
//	float 			azimuth		// sonic azimuth
//	float				declination 	// location declination
//	STRUCT CBSV 	wsVavg 		// ws vector average options
//	STRUCT CBSV 	wsSavg 		// ws scalar average options
//	STRUCT CBSV 	wsSsdev 	// ws scalar std dev options
//	STRUCT CBSV 	wsPers		// ws persistence options
//	STRUCT CBSV 	wdVavg		// wd vector average options
//	STRUCT CBSV 	wdSavg 		// wd scalar average options
//	STRUCT CBSV 	wdSsdev 	// wd scalar std dev options
//	STRUCT CBSV 	wdVsdevY	// wd vector std dev yamartino options
//	STRUCT CBSV 	wdVsdevM 	// wd vectory std dev mardia options
//	uchar 			wildcardStr[MAX_OBJ_NAME] // replace wildcard * with this string in names
//	STRUCT CBSV	winSize		// averaging window size (seconds)
//	STRUCT CBSV 	saveIn 		// output path options
//	uchar			maxEmpty 	// max allowable % of nans per window 
//EndStructure
//// restores preferences to wswdPanel
//Static Function lar_wswdLoadPrefs(prefs, [reset])
//	STRUCT lar_wswdPrefs &prefs
//	variable reset							// optional: nonzero to reset to defaults
//	variable currentVersion = 20101110		// YYYYMMDD date struct last changed
//	
//	LoadPackagePreferences ksPackageName, ksPrefsFileName, kID_wswd, prefs
//	if ( V_flag != 0 || V_bytesRead == 0 || prefs.version != currentVersion || reset ) // if prefs no good
//		prefs.version = currentVersion				// fill them with this version
//		prefs.panel.left = 108						// dot notion to access substructure
//		prefs.panel.top = 141
//		prefs.panel.right = 378 					// start with right edge covering equations 526=open
//		prefs.panel.bottom = 701
//		prefs.Uname = "U"
//		prefs.Vname = "V"
//		prefs.azimuth = 0
//		prefs.declination = 0
//		prefs.wsVavg.checked = 1
//		prefs.wsVavg.isString = 1
//		prefs.wsVavg.strVal = "WS_vctr_*"
//		prefs.wsSavg.checked = 1
//		prefs.wsSavg.isString = 1
//		prefs.wsSavg.strVal = "WS_sclr_*"
//		prefs.wsSsdev.checked = 1
//		prefs.wsSsdev.isString = 1
//		prefs.wsSsdev.strVal = "WS_sclr_*_sdev"
//		prefs.wsPers.checked = 0
//		prefs.wsPers.isString = 1
//		prefs.wsPers.strVal = "WS_pers_*"
//		prefs.wdVavg.checked = 1
//		prefs.wdVavg.isString = 1
//		prefs.wdVavg.strVal = "WD_vctr_*"
//		prefs.wdSavg.checked = 0
//		prefs.wdSavg.isString = 1
//		prefs.wdSavg.strVal = "WD_sclr_*"
//		prefs.wdSsdev.checked = 0
//		prefs.wdSsdev.isString = 1
//		prefs.wdSsdev.strVal = "WD_sclr_*_sdev"
//		prefs.wdVsdevY.checked = 0
//		prefs.wdVsdevY.isString = 1
//		prefs.wdVsdevY.strVal = "WD_vctr_*_sdevY"
//		prefs.wdVsdevM.checked = 0
//		prefs.wdVsdevM.isString = 1
//		prefs.wdVsdevM.strVal = "WD_vctr_*_sdevM"
//		prefs.wildcardStr = "1"
//		prefs.winSize.numVal = 300
//		prefs.winSize.checked = 1
//		prefs.saveIn.checked = 1
//		prefs.saveIn.isString = 1
//		prefs.saveIn.strVal = ":"
//		prefs.maxEmpty = 100
//		
//		lar_wswdSavePrefs(prefs)			// create new prefs file
//	endif
//	return 0
//End
//// saves preferences for function
//Static Function lar_wswdSavePrefs(prefs)
//	STRUCT lar_wswdPrefs &prefs
//	SavePackagePreferences ksPackageName, ksPrefsFileName, kID_wswd, prefs
//	return (V_flag)
//End
//// resets preferences to default values
//Static Function lar_wswdResetPrefs()
//	STRUCT lar_wswdPrefs prefs
//	// lar_print(1, "Panel preferences reset for sonic to ws/wd")
//	return lar_wswdLoadPrefs(prefs, reset=1)
//End
//// handles preferences concerning panel appearance, position, etc
//Static Function lar_wswdPanelHook(s)
//	STRUCT WMWinHookStruct &s				// window passed by ref
//	switch (s.eventCode)							// what happened?
//		case 0: 										// activate
//			PopupMenu inFolder, mode=(1+WhichListItem(GetDataFolder(1), GetDataFolderList())) // set popup to current DF
//		case 6:										// resized
//		case 12:										// moved
//			STRUCT lar_wswdPrefs prefs					// invoke function preferences
//			lar_wswdLoadPrefs(prefs)						// look up current prefs
//			GetWindow wswdPanel wsize					// grab new position info
//			variable scale = ScreenResolution / 72 			// convert points to device units
//			prefs.panel.left = V_left * scale					// save to prefs
//			prefs.panel.top = V_top * scale
//			prefs.panel.right = V_right * scale
//			prefs.panel.bottom = V_bottom * scale
//			lar_wswdSavePrefs(prefs)						// save prefs
//			break
//	endswitch
//	return 0
//End
//// Handles buttons on wswdPanel
//Static Function lar_wswdBtnProc(ba) : ButtonControl
//	STRUCT WMButtonAction &ba 			// who called this? pass-by-ref
//	switch( ba.eventCode ) 					// what happened?
//		case 2: 									// mouse up
//			STRUCT lar_wswdPrefs prefs				// invoke panel prefs
//			lar_wswdLoadPrefs(prefs)					// populate
//			strswitch( ba.ctrlName )					// ask for control's name
//				case "submitB":							// submit
//					//	
//					break
//				case "formulaB": 							// hide/show formulas
//					ControlInfo/W=$ba.win formulaB			// get detailed info
////					strswitch ( S_userdata )					// check for title stored in userdata
////						case "Show equations":					// if eqns are hidden
////							print "showing equations"
////							MoveWindow/W=wswdPanel prefs.panel.left, prefs.panel.top, prefs.panel.right, prefs.panel.bottom 
////							prefs.eqnState = "Hide equations"			// change prefs & button title
////							Button formulaB,win=$ba.win,title=prefs.eqnState,userdata=prefs.eqnState
////							break
////						case "Hide equations": 					// if eqns are shown
////							print "hiding equations"
////							MoveWindow/W=wswdPanel prefs.panel.left, prefs.panel.top, prefs.panel.right, prefs.panel.bottom 
////							prefs.eqnState = "Show equations"			// change prefs & button title
////							Button formulaB,win=$ba.win,title=prefs.eqnState,userdata=prefs.eqnState
////							break
////					endswitch
//					break
//				default:
//					// do nothing
//					break
//			endswitch
//			lar_wswdSavePrefs(prefs) 					// save prefs
//			break
//	endswitch
//End
//// Handles checkboxes on wswdPanel
//Static Function lar_wswdChkProc(cb) : CheckBoxControl
//	STRUCT WMCheckboxAction &cb 						// pass-by-ref who called?
//	switch( cb.eventCode ) 								// what happened
//		case 2: 												// mouse up
//			STRUCT lar_wswdPrefs prefs							// invoke panel prefs
//			lar_wswdLoadPrefs(prefs)								// populate them
//			strswitch( cb.ctrlName )								// ask for a name
//				case "wsVavgCB":									// ws vector average
//					prefs.wsVavg.checked = cb.checked 					// save change to prefs
//					SetVariable wsVavgSV,win=$cb.win,disable= (prefs.wsVavg.checked ? 0 : 2) // enable/disable
//					break
//				case "wsSavgCB": 									// ws scalar average
//					prefs.wsSavg.checked = cb.checked 
//					SetVariable wsSavgSV,win=$cb.win,disable= (prefs.wsSavg.checked ? 0 : 2) 
//					break
//				case "wsSsdevCB": 									// ws scalar std dev
//					prefs.wsSsdev.checked = cb.checked
//					SetVariable wsSsdevSV,win=$cb.win,disable= (prefs.wsSsdev.checked ? 0 : 2)
//					break
//				case "wsPersCB": 									// ws persistence
//					prefs.wsPers.checked = cb.checked
//					SetVariable wsPersSV,win=$cb.win,disable= (prefs.wsPers.checked ? 0 : 2)
//					break
//				case "wdVavgCB": 									// wd vector average
//					prefs.wdVavg.checked = cb.checked
//					SetVariable wdVavgSV,win=$cb.win,disable= (prefs.wdVavg.checked ? 0 : 2)
//					break
//				case "wdSavgCB": 									// wd scalar average
//					prefs.wdSavg.checked = cb.checked
//					SetVariable wdSavgSV,win=$cb.win,disable= (prefs.wdSavg.checked ? 0 : 2)
//					break
//				case "wdSsdevCB": 									// wd scalar std dev
//					prefs.wdSsdev.checked = cb.checked
//					SetVariable wdSsdevSV,win=$cb.win,disable= (prefs.wdSsdev.checked ? 0 : 2)
//					break
//				case "wdVsdevYCB": 									// wd vector std dev by yamartino
//					prefs.wdVsdevY.checked = cb.checked
//					SetVariable wdVsdevYSV,win=$cb.win,disable= (prefs.wdVsdevY.checked ? 0 : 2)
//					break
//				case "wdVsdevMCB": 								// wd vector std dev by mardia
//					prefs.wdVsdevM.checked = cb.checked
//					SetVariable wdVsdevMSV,win=$cb.win,disable= (prefs.wdVsdevM.checked ? 0 : 2)
//					break
//				case "winsizeCB": 									// nice intervals?
//					prefs.winsize.checked = cb.checked
//					// don't enable/disable anything
//					break
//				case "saveinCB":										// save in location
//					prefs.saveIn.checked = cb.checked
//					SetVariable saveinSV,win=$cb.win,disable= (prefs.savein.checked ? 0 : 2)
//					break
//				default:
//					// do nothing
//					break
//			endswitch
//			lar_wswdSavePrefs(prefs) 							// save preferences
//			break
//		case -1:											// control killed
//		default:
//			break
//	endswitch
//	return 0
//End
//// handles popup menus on wswdPanel
//Static Function lar_wswdPopupProc(pa) : PopupMenuControl
//	STRUCT WMPopupAction &pa							// pass caller by ref 
//	switch( pa.eventCode )								// what happened?
//		case 2: 												// mouse up
//			STRUCT lar_wswdPrefs prefs 							// invoke prefs
//			lar_wswdLoadPrefs(prefs) 								// populate them
//			strswitch( pa.ctrlName )								// now who called?
//				case "inFolder":										// folder selection popup
//					if ( DataFolderExists(pa.popStr) )						// if directory exists
//						SetDataFolder $pa.popStr								// switch to
//					else													// otherwise
//						// look up index # of current datafolder and set popup value to
//						PopupMenu inFolder,win=$pa.win,mode=(Max( (1+WhichListItem(GetDataFolder(1), GetDataFolderList())),1))
//					endif
//					break
//				case "uPU": 											// U-wave popup
//					if ( WaveExists($pa.popStr) )							// if wave exists here
//						prefs.Uname = pa.popStr								// save change to prefs
//					else 												// otherwise 
//						// look up index # of current pref and set to
//						PopupMenu uPU,win=$pa.win,mode=(Max( (1+WhichListItem(prefs.Uname, WaveList("*",";","TEXT:0"))), 1 ))
//					endif
//					break
//				case "vPU": 											// V-wave popup
//					if ( WaveExists($pa.popStr) )							// if wave still exists
//						prefs.Vname = pa.popStr 								// save to prefs
//					else 												// otherwise
//						// look up index # of current pref and set to
//						PopupMenu vPU,win=$pa.win,mode=(Max( (1+WhichListItem(prefs.Vname, WaveList("*",";","TEXT:0"))), 1 ))
//					endif
//					break
//			endswitch
//			lar_wswdSavePrefs(prefs)								// save changes
//			break
//		case -1: 												// control being killed
//		default: 
//			break
//	endswitch
//	return 0
//End		
//// handles set variables on wswdPanel
//Static Function lar_wswdSetVProc(sv) : SetVariableControl
//	STRUCT WMSetVariableAction &sv					// pass caller by ref
//	switch( sv.eventCode )								// what happened
//		case 1: 											// mouse up
//		case 2: 											// Enter key
//		case 3:											// live update
//		case 6: 											// value changed by dependency
//			STRUCT lar_wswdPrefs prefs						// invoke prefs
//			lar_wswdLoadPrefs(prefs)							// populate them
//			strswitch ( sv.ctrlName ) 							// who called?
//				case "azimuthSV":								// azimuth 
//					prefs.azimuth = sv.dval							// save to prefs
//					break
//				case "declinationSV":								// declination
//					prefs.declination = sv.dval							// save to prefs
//					break
//				case "wsVavgSV":								// ws vector avg
//					prefs.wsVavg.strVal = sv.sval						// save to prefs
//					break
//				case "wsSavgSV":								// ws scalar avg
//					prefs.wsSavg.strVal = sv.sval						// save to prefs
//					break
//				case "wsSsdevSV":								// ws scalar std dev
//					prefs.wsSsdev.strVal = sv.sval						// save to prefs
//					break
//				case "wsPersSV":								// ws persistence
//					prefs.wsPers.strVal = sv.sval						// save to prefs
//					break
//				case "wdVavgSV":								// wd vector avg
//					prefs.wdVavg.strVal = sv.sval						// save to prefs
//					break
//				case "wdSavgSV":								// wd scalar avg
//					prefs.wdSavg.strVal = sv.sval						// save to prefs
//					break
//				case "wdSsdevSV":								// wd scalar std dev
//					prefs.wdSsdev.strVal = sv.sval						// save to prefs
//					break
//				case "wdVsdevYSV":								// wd vector std dev Yamartino est
//					prefs.wdVsdevY.strVal = sv.sval					// save to prefs
//					break
//				case "wdVsdevMSV":								// wd vector std dev Mardia est
//					prefs.wdVsdevM.strVal = sv.sval					// save to prefs
//					break
//				case "wildcardSV": 								// wild card string
//					prefs.wildcardStr = sv.sval							// save to prefs
//					break
//				case "winsizeSV": 								// averaging window size
//					prefs.winSize.numVal = sv.dval						// save to prefs
//					break
//				case "saveinSV": 								// target save directory
//					prefs.saveIn.strVal = sv.sval						// save to prefs
//					break
//				case "maxemptySV":								// max % missing points
//					prefs.maxEmpty = sv.dval							// save to prefs
//					break
//				default:
//					// do nothing
//					break
//			endswitch
//			lar_wswdSavePrefs(prefs)							// retain changes
//			break
//	endswitch
//	return 0
//End



//// Define indexes for tabs on main panel
//Static Constant kFil = 0								// Files
//Static Constant kTim = 1 								// Time
//Static Constant kWav = 2 								// Waves
//Static Constant kSta = 3 								// Stats
//Static Constant kSet = 4 								// Settings
//
//// Define text names for each index
//Static StrConstant ksFil = "Files"
//Static StrConstant ksTim = "Time"
//Static StrConstant ksWav = "Waves"
//Static StrConstant ksSta = "Stats"
//Static StrConstant ksSet = "Settings"
//
//------------------------------------------------------------------------------------------------------------------------------------------------------
// A re-created panel to demonstrate panel code snippets below
//Function LARpanel() : Panel
//	DoWindow/F examplepanel		// attempt to bring to focus
//	If (V_flag != 0) 					// if succeeded,
//		return 0							// quit successfully
//	endif
//	
//	PauseUpdate; Silent 1		// building window...
//	NewPanel/K=1/W=(563,386,915,795)/N=examplepanel
//	
//	// tabs layout
//	TabControl panelTabs,pos={10,54},size={330,342},proc=examples#panelTabProc,value=0
//	TabControl panelTabs,tabLabel(kFil)=ksFil,tabLabel(kTim)=ksTim,tabLabel(kWav)=ksWav,tabLabel(kSta)=ksSta,tabLabel(kSet)=ksSet
//	
//	// folder selection
//	PopupMenu pop_currentDF,pos={12,15},size={145,21},proc=examples#puFolderSwitch,title="Current Datafolder: "
//	PopupMenu pop_currentDF,mode=1,popvalue="root:",value= #"examples#GetDataFolderList()"
//	
//	// file import tab
//	GroupBox fil_grp_fileimport,pos={22,85},size={300,273},title="File Import",frame=0
//	PopupMenu fil_pop_filetype,pos={31,104},size={150,21},title="File type: "
//	PopupMenu fil_pop_filetype,value= #"\"Plain text (CSV);Plain text (TAB);CSI Long header (TOAB1);CSI Short header (TOA5);CSI whatever;Picarro G2301 [CO2/CH4/H2O];Picarro G1103 [NH3];Los Gatos DLT-100 [N2O/CO];Generic GPS (NMEA)\""
//	PopupMenu fil_pop_filetype,proc=examples#panelPopProc
//	SetVariable fil_sv_destfldr,pos={31,222},size={283,16},title="Destination folder: ",value= _STR:"root:"
//	Button fil_btn_setcurr,pos={234,245},size={80,20},title="Set to current",proc=examples#panelBtnProc
//	CheckBox fil_chk_derivedest,pos={42,249},size={175,14},title="Derive folder name from file name",value= 0,proc=examples#panelChkProc
//	CheckBox fil_chk_autoname,pos={80,136},size={66,14},title="Autoname",value= 0,proc=examples#panelChkProc
//	CheckBox fil_chk_overwrite,pos={80,153},size={63,14},title="Overwrite",value= 0,proc=examples#panelChkProc
//	CheckBox fil_chk_quiet,pos={188,137},size={76,14},title="Quiet results",value= 0,proc=examples#panelChkProc
//	TitleBox fil_title_options,pos={181,40},size={50,20}
//	Button fil_btn_loadfiles,pos={276,366},size={50,20},title="Do it",proc=examples#panelBtnProc
//	
//	// time tab
//	GroupBox tim_grp_convert,pos={18,87},size={315,147},title="Convert "
//	TitleBox tim_title_convert,pos={49,177},size={40,13},title="Title box",frame=0
//	CheckBox tim_chk_exceltime,pos={27,111},size={296,14},title="Convert serial dates to Igor date/time? (Excel/DAQFactory)", value= 0
//
//	// waves tab
//	
//	// stats tab
//
//	// settings tab
//	GroupBox set_grp_pref,pos={22,84},size={308,160},title="Preferences"
//	PopupMenu set_pop_verb,pos={32,108},size={159,21},title="Set verbosity level: ",proc=examples#setverbosity
//	PopupMenu set_pop_verb,value= #"\"0 Silent;1 Error only;2 Finish summary;3 Basic update;4 Detail update;5 Debug\""
//	TitleBox set_title_verb,pos={241,65},size={50,20}
//	GroupBox set_grp_about,pos={25,251},size={305,131},title="About"
//	TitleBox set_title_author,pos={37,278},size={114,13},title="Author: Patrick O'Keeffe"
//	TitleBox set_title_author,frame=0
//	TitleBox set_title_contact,pos={37,294},size={192,13},title="Contact: patrick.okeeffe@email.wsu.edu"
//	TitleBox set_title_contact,frame=0
//	TitleBox set_title_version,pos={38,310},size={116,13},title="Version: 20110308.1624"
//	TitleBox set_title_version,frame=0
//	TitleBox set_title_updated,pos={38,326},size={191,13},title="Last updated: March 8, 2010 16:24 PST"
//	TitleBox set_title_updated,frame=0
//	TitleBox set_title_about,pos={39,358},size={67,13},title="About title box"
//	TitleBox set_title_about,frame=0
//	
//	// final refresh and values update
//	panelShowTab(kFil)	
//End
//
//
//// panelTabSwitch( tca )
////	tca 		TabControlAction structure
////
//// returns 0 for success
////
//// Handles reaction of pressing tabs (ie calls function to show/hide)
//Static Function panelTabSwitch(tca) : TabControl
//	STRUCT WMTabControlAction &tca			// tab structure passed from panel
//	switch( tca.eventCode )						// what happened to the tab?
//		case 2: 										// mouse up
//			panelShowTab( tca.tab ) 					// show/hide controls
//			break
//		endswitch
//	return 0
//End
//
//// panelShowTab( tab )
////	tab 		index # of tab to be displayed
////
//// returns index # of tab displayed
////
//// Switches settings on panel controls to show/hide according to tab group
//Static Function panelShowTab( tab )
//	variable tab 									// index # of tab to show
//	variable flag 									// boolean flag for show/hide
//			
//	flag = (tab != kFil)								// flag = true if tab # is not Files tab #
//	GroupBox fil_grp_fileimport, 	disable = flag
//	PopupMenu fil_pop_filetype, 	disable = flag
//	SetVariable fil_sv_destfldr, 	disable = flag 
//	Button fil_btn_setcurr, 		disable = flag
//	CheckBox fil_chk_derivedest, 	disable = flag
//	CheckBox fil_chk_autoname, 	disable = flag
//	CheckBox fil_chk_overwrite, 	disable = flag
//	CheckBox fil_chk_quiet, 		disable = flag
//	TitleBox fil_title_options, 		disable = flag
//	Button fil_btn_loadfiles, 		disable = flag
//	
//	flag = (tab != kTim) 							// flag = true if tab # is not Time tab #
//	GroupBox tim_grp_convert, 	disable = flag
//	TitleBox tim_title_convert, 		disable = flag
//	CheckBox tim_chk_exceltime, 	disable = flag
//	
//	flag = (tab != kWav) 							// flag = true if tab # is not Waves tab #
//	
//	flag = (tab != kSta) 							// flag = true if tab # is not Stats tab #
//	
//	flag = (tab != kSet) 							// flag = true if tab # is not Settings tab #
//	GroupBox set_grp_pref, 		disable = flag
//	PopupMenu set_pop_verb, 	disable = flag
//	TitleBox set_title_verb, 		disable = flag
//	GroupBox set_grp_about,		disable = flag
//	TitleBox set_title_author, 		disable = flag
//	TitleBox set_title_contact, 		disable = flag
//	TitleBox set_title_version, 		disable = flag
//	TitleBox set_title_updated,		disable = flag
//	TitleBox set_title_about, 		disable = flag
//			
//	panelRefreshTab(tab)
//	return tab
//End
//
//// panelRefreshTab( tab )
//// 	tab 		index # of tab to refresh controls of
//// 	
//// returns index # of tab refreshed
//Static Function panelRefreshTab( tab )
//	variable tab 									// index of tab to refresh
//	NVAR K19 									// global verbosity value
//	
//	// update data folder drop down every time	
//	PopupMenu pop_currentDF, mode=(Max((1+WhichListItem(GetDataFolder(1), GetDataFolderList())), 1))
//	
//	// based on currently displayed tab, update fields as needed:
//	switch ( tab )
//		case kFil: 								// Files
//			
//			break
//		case kTim:								// Time
//			
//			break
//		case kWav: 								// Waves
//			
//			break
//		case kSta:								// Stats
//			
//			break
//		case kSet:								// Settings
//			PopupMenu set_pop_verb, mode=(1+K19)
//			TitleBox set_title_author, title="Author: "+ksAuthor
//			TitleBox set_title_contact, title="Contact: "+ksContact
//			TitleBox set_title_version, title="Version: "+ksVersion
//			TitleBox set_title_updated, title="Last updated: "+ksUpdated
//			break
//	endswitch
//	return tab
//End
//
//// panelHook( hs )
////	hs 		Window Hook structure
////
//// returns 0 for success
////
//// Function is called with every interaction of the window; used mostly to refresh values & save position prefs
//Static Function panelHook(hs)
//	STRUCT WMWinHookStruct &hs				// window passed by ref
//	//STRUCT lar_textToIgorTimePrefs prefs			// invoke prefs
//	//lar_textToIgorTimeLoadPrefs(prefs)				// populate
//	switch (hs.eventCode)							// what happened?
//		case 0: 										// activate
//			ControlInfo/W=$hs.winName panelTabs			// ask tab group which tab is showing
//			panelRefreshTab(V_value)						// update displayed values to current
//		case 6:										// resized
//		case 12:										// moved
////			GetWindow $s.winName wsize					// grab new position info
////			variable scale = ScreenResolution / 72			// scale points to device units
////			prefs.panel.left = V_left * scale					// save to prefs
////			prefs.panel.top = V_top * scale
////			prefs.panel.right = V_right * scale
////			prefs.panel.bottom = V_bottom * scale
////			lar_textToIgorTimeSavePrefs(prefs)				// save prefs
//			break
//	endswitch
//	return 0
//End








//// Posted Nov 3, 2009 by harneit [Wolfgang Harneit] at http://www.igorexchange.com/node/1228
////
//// function AppendWaves(parentDF, DFpattern, killChildDFs, [sortedAppend, sortOptions, modelDF])
////
//// 	Given a data folder parentDF that contains wave1, Vwave and w3, AppendWaves appends
////	waves with the same name (i.e. wave1, Vwave and w3) found in any child data folder of parentDF, 
////	if those childDFs match a name pattern DFpattern. If killChildDFs is true (non-zero), the childDFs 
////	from which data were appended are killed. The sequence in which data folders are cycled (and thus
////	data are appended) can be influenced by setting sortedAppend=1 and sortOptions (see help for
////	SortList). If parentDF is empty initially, a modelDF may be specified to determine which waves are
////	to be extracted by AppendWave.
////
////	- if parentDF is an empty string, AppendWaves uses the current data folder
////	- if DFpattern is an empty string, AppendWaves uses the match-all pattern "*"
////	- if killChildDFs is non-zero, AppendWaves will kill child data folders with "exploited" data
////	- if sortedAppend is non-zero, AppendWaves will sort the child data folders before appending
////	- if sortedAppend is non-zero, sortOptions as used by SortList may be specified
////	- if modelDF is specified, AppendWaves uses it to determine which waves should be appended
////	- if modelDF is not specified, AppendWaves uses the parentDF as a model instead
////
////	Thus, AppendWaves("", "", 0) called with "root:" as the current data folder tries to append to all 
//// 	waves currently living in "root:"; it collects waves from all top-level data folders and then kills those 
////	data folders from which it collected waves. Use these "wildcards" with care...
////
////	The intended use is to load e.g. time series data spread over many files into separate folders called
////	"day1", "day2", etc. The folders can be put into a master folder called "alldays" so that we have a
////	folder structure like this:
////	>alldays
////	>>day1
////	>>--wave1 (0,1,2)
////	>>--wave2 (2,3,4)
////	>>day2
////	>>--wave1 (100, 101, 102)
////	>>--wave2 (202, 203, 204)
////	Now, call AppendWaves("alldays", "day*", 1) in order to collapse all runs into one:
////	>alldays
////	>--wave1 (0,1,2,100,101,102)
////	>--wave2 (2,3,4,202,203,204)
////
//Static function AppendWaves(parentDF, DFpattern, killChildDFs, [sortedAppend, sortOptions, modelDF])
//	string parentDF, DFpattern
//	variable killChildDFs, sortedAppend, sortOptions
//	string modelDF
//	// get list of childDFs matching DFpattern
//	if( strlen(parentDF) == 0 )
//		parentDF = GetDataFolder(1)
//	endif
//	string childDFs = ListMatch( ChildDFList(parentDF), DFpattern+"*" )
//	if( sortedAppend )		// default is 0 = false
//		childDFs = SortList(childDFs, ";", sortOptions)	// default is 0 = ascending case-sensitive 
//	endif											// alphabetic ASCII sort
//	variable numChildren = ItemsInList(childDFs)		// count the "good" children
//	if( numChildren == 0 )						// ...none -- abort
//		Abort "AppendWave found no child data folders"
//	endif
//	// build WList from waves in modelDF or in parentDF
//	string WList, saveDF = GetDataFolder(1)
//	if( !ParamIsDefault(modelDF) )	// use modelDF if specified
//		SetDataFolder modelDF
//		WList = WaveList("*", ";", "")
//	else
//		SetDataFolder parentDF	// default to parentDF
//		WList = WaveList("*", ";", "")
//	endif
//	variable numWaves = ItemsInList(WList)		// count waves to append to
//	if( numWaves == 0 )						// ...none -- abort
//		Abort "AppendWave found no waves to append to in data folder \""+GetDataFolder(1)+"\""
//	endif
// 
//	variable k, m, childDataAppended
//	string childDF, currentWave, sourceWave, destWave
//	// cycle through childDFs
//	for( k = 0; k < numChildren; k += 1 )
//		childDF = StringFromList(k, childDFs)
//		SetDataFolder childDF
//		childDataAppended = 0	// don't want to kill childDFs that contained no data to append
//		// ... cycle through WList
//		for( m = 0; m < numWaves; m += 1 )
//			currentWave = StringFromList(m, WList)
//			sourceWave = childDF+currentWave
//			if( exists(sourceWave) == 1 )	// not all waves in child data folder may be relevant
//				destWave = parentDF+currentWave
//				if( exists(destWave) == 1 )	// parentDF may be empty initially
//					Concatenate/NP sourceWave+";", $destWave
//					print "C "+sourceWave+","+destWave
//				else
//					Duplicate $sourceWave, $destWave
//					print "D "+sourceWave+","+destWave
//				endif
//				childDataAppended = 1
//			endif
//		endfor
//		if( childDataAppended && killChildDFs )
//			KillDataFolder childDF
//		endif
//	endfor
//	SetDataFolder saveDF
//end
////
//// function/S ChildDFList(parentDF) 
//// returns a list of full paths to child data folders of data folder specified by parentDF
////	* if parentDF is an empty string, ChildDFList uses the current data folder 
//Static function/S ChildDFList(parentDF)
//	string parentDF
//	if( strlen(parentDF) == 0 )
//		parentDF = GetDataFolder(1)
//	elseif( !DataFolderExists(parentDF) )
//		Abort "ChildDFList cannot find specified data folder \""+parentDF+"\""
//	endif
//	parentDF = RemoveEnding(parentDF, ":") + ":"	// make sure parentDF ends in a colon
//	variable k
//	string CDFList = ""
//	for( k = 0; k < CountObjects(parentDF,4); k += 1)
//		CDFList += parentDF+GetIndexedObjName(parentDF, 4, k)+":;"
//	endfor
//	return CDFList
//end





//// Posted January 30th, 2010 by nplumb in Igor Exchange Snippets
////
//Static Function OpenDragNDropExample()
//	Make/O/T/N=4 root:testList = {"item 1", "item 2", "item 3", "item 4"}
//	Make/O/N=4 root:testSel
//	DoWindow/K Panel_DragNDrop
//	Execute/Q "Panel_DragNDrop()"
//End
//Window Panel_DragNDrop() : Panel
//	PauseUpdate; Silent 1		// building window...
//	NewPanel /W=(350,125,650,325) as "Drag List Items Example"
//	ListBox list0,pos={1,2},size={298,197},proc=ListBoxProc_DragNDropLB
//	ListBox list0,listWave=root:testList,selWave=root:testSel,mode= 1,selRow= 1
//EndMacro
//Static Function ListBoxProc_DragNDropLB(lba) : ListBoxControl
//	STRUCT WMListboxAction &lba
// 
//	Variable row = lba.row
//	Variable col = lba.col
//	WAVE/T/Z listWave = lba.listWave
//	WAVE/Z selWave = lba.selWave
// 
//	switch( lba.eventCode )
//		case -1: // control being killed
//			break
//		case 1: // mouse down
//			Variable/G V_MouseDownRow = row
//			break
//		case 2: // mouse up
//			if(row != V_MouseDownRow)						// dragged?
//				NVAR V_MouseDownRow
//				String item = listWave[V_MouseDownRow]
//				DeletePoints V_MouseDownRow, 1, listWave	// do swap
//				InsertPoints row, 1, listWave
//				listWave[row] = item
//			endif
//			KillVariables V_MouseDownRow	// cleanup variable
//			break
//		case 3: // double click
//			break
//		case 4: // cell selection
//		case 5: // cell selection plus shift key
//			break
//		case 6: // begin edit
//			break
//		case 7: // finish edit
//			break
//	endswitch
// 
//	return 0
//End
//
//






#EndIf