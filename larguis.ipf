//=====================================================================================
//	LAR Graphical User Interfaces
//	
//	Patrick O'Keeffe 					Laboratory for Atmospheric Research 
// 	patrick.okeeffe@email.wsu.edu		Dept. of Civil & Environmental Engineering 
// 	509.335.7246 					Washington State University 
//
//=====================================================================================
#pragma rtGlobals = 1				// Use modern global access method.
#pragma IgorVersion = 6.20
#pragma version = 0.20111128		// this field last changed on, YYYYMMDD

#If ( 1 )							// 1 to compile; 0 to disable procedure
//=====================================================================================
//					INCLUDES  
//=====================================================================================



//=====================================================================================
//					CONSTANTS  
//=====================================================================================
StrConstant ksFileFilter = "Plain-text data (.txt .dat .csv .log):.txt,.dat,.csv,.log;Archived data (.zip .7z .rar):.zip,.7z,.rar;All files (*.*):.*;"

// as defined for use in structures
Constant MAX_OBJ_NAME  = 31		// names
Constant MAX_WIN_PATH = 199		// windows
Constant MAX_UNITS = 49			// axis units
Constant MAXCMDLEN = 400		// string values



//=====================================================================================
//					MENUS 
//=====================================================================================
// escape characters
// 	/Q 		supresses printing history to command window
// 	\\M0	deactivates all special characters
// 	\\M0:: 	interpret only special characters between colons
// special characters
//		/		designate command key for menu item
//		(		disable menu item
//		!		mark menu item with next char (displays checkmark regardless)
//		&		following character is mnemonic keystroke, accessible via Alt on windows only
// help strings are displayed in the status bar, lower left
//
// Built-in menus that can be extended:
// 	Add Controls 	AllTracesPopup 		Analysis 	Append to Graph 		Control 			Data
//	Edit 			File 				 	Graph 		GraphMarquee		Help 			Layout
// 	LayoutMarquee 	Load Waves 			Macros 		Misc 				Statistics 		New 
// 	Notebook		Open File			Panel 		Procedure 			Save Waves 		Table
// 	TracePopup 
//

Menu "GraphMarquee"
	Submenu "Autoscale"
		"Left axis", SetAxis/A/Z left
		"Bottom axis", SetAxis/A/Z bottom
		"Right axis", SetAxis/A/Z right
		"Top axis", SetAxis/A/Z top
	End
	Submenu "Autoscale visible"
		"Left axis", SetAxis/A=2/Z left
		"Right axis", SetAxis/A=2/Z right
		"Both", SetAxis/A=2/Z left; SetAxis/A=2/Z right
	End
	Submenu "Copy axis ranges"
		"from left to right", GetAxis/Q left; SetAxis/Z right V_min, V_max
		"from right to left", GetAxis/Q right; SetAxis/Z left V_min, V_max
		"from bottom to top", GetAxis/Q bottom; SetAxis/Z top V_min, V_max
		"from top to bottom", GetAxis/Q top; SetAxis/Z bottom V_min, V_max
	End
End

Menu "GraphMarquee", dynamic
	Submenu "Set to NAN in marquee"
		Submenu "using all bounds"
			SetMarqueeToNanMenu(0),/Q, SetMarqueeToNaN(0)
		End
		Submenu "using L-R bounds"
			SetMarqueeToNanMenu(1),/Q, SetMarqueeToNaN(1)
		End
		Submenu "using T-B bounds"
			SetMarqueeToNanMenu(2),/Q, SetMarqueeToNaN(2)
		End
	End
End

Menu "TracePopup"
	Submenu "Autoscale"
		"Left axis", SetAxis/A left
		"Bottom axis", SetAxis/A bottom
		"Right axis", SetAxis/A right
		"Top axis", SetAxis/A top
	End
	Submenu "Autoscale visible"
		"Left axis", SetAxis/A=2 left
		"Right axis", SetAxis/A=2 right
		"Both", SetAxis/A=2/Z left; SetAxis/A=2/Z right
	End
	Submenu "Copy axis ranges"
		"from left to right", GetAxis/Q left; SetAxis/Z right V_min, V_max
		"from right to left", GetAxis/Q right; SetAxis/Z left V_min, V_max
		"from bottom to top", GetAxis/Q bottom; SetAxis/Z top V_min, V_max
		"from top to bottom", GetAxis/Q top; SetAxis/Z bottom V_min, V_max
	End
End

Menu "Notebook"
	"&Unmake help link", /Q, Notebook kwTopWin textRGB=(0,0,0), fStyle=-1
		help = {"Makes it regular text (not underlined, blue)"}
End

Menu "Load Waves"
	"\\M0:/1:Load CampbellSci TOA5 (long header)...", /Q, PromptedLoadCSI()
		help = {"Loads data from Campbell Scientific long-header (TOA5) file(s)"}
	"\\M0::Load CampbellSci TOACI1 (short header)...", /Q, PromptedLoadCSI()
		help = {"Loads data from Campbell Scientific short-header (TOACI1) file(s)"}
	"\\M0::Load Los Gatos DLT-100...", /Q, LoadLGR_Panel()
		help = {"Launch dialog to load Los Gatos Research Fast N2O Analyzer file(s)"}
//	"\\M0::Load Picarro G1103...", /Q, beep
//		help = {"Loads data from Picarro Fast-Ammonia Analyzer file(s)"}
	"\\M0:!*:Load Picarro G2301...",  /Q, LoadG2301()
		help = {"Loads data from Picarro Fast-Methane Analyzer file(s)"}
//	"\\M0::Load Generic GPS...", /Q, LoadNMEA()
//		help = {"Loads data from NMEA-formatted text file"}
End

// add our own menu
//Menu "A&RT"
//	Submenu "Timestamps"
//		"\\M0::Convert text timestamps into Igor date/time", /Q, lar_tools#textToIgorTimePanel()
//			help = {"Parse date & time from various timestamp formats loaded as text waves"}	
//		"\\M0:(:Convert Excel/DAQFactory timestamp to Igor date/time", /Q, beep
//			help = {"This would be a nice function to have (EVENTUALLY)"}
//		"\\M0:(:Create diel (time-of-day) timestamps", /Q, beep
//			help = {"This would be a nice function to have (EVENTUALLY)"}
//		"\\M0:(:Create day-of-week timestamps", /Q, beep
//			help = {"This would be a nice function to have (EVENTUALLY)"}
//		"\\M0:(:Shift interval timestamp (starting<-->midpoint<-->ending)", /Q, beep
//			help = {"This would be a nice function to have (EVENTUALLY)"}
//	End
//	Submenu "Wind Tools"
//		"\\M0::Interval wind statistics from sonic anemometer", /Q, Interval_windStats()
//			help = {"Calculate wind speed/direction in specified intervals from instant sonic anemometer U, V components"}
//		"\\M0::Interval rotation from sonic to stream-wise coordinates", /Q, Interval_uvwRotation()
//			help = {"Rotate instant UVW components by double/triple rotation or planar fit over specified intervals"}
//		"-"
//		"\\M0:(:Plot timeseries of arrows", /Q
//		"\\M0:(:Plot polar histogram of WD", /Q
//		"\\M0:(:Plot polar histogram of scalar versus WD", /Q
//		"\\M0:!*:Plot time series of wind arrows", windArrowTimeSeries_tb()
//		"\\M0:!*:       reshow markers -> arrows command", showArrowMarkerCmd_tb()
//		"\\M0:!*:(Polar plot of scalar vs. WD", polarWDvsScaler_tb()
//		"\\M0:!*:(Bin-average polar plot of scalar vs. WD", binAvgPolarWDvsScalar_tb()
//	End
//	Submenu "Map tools"
//		"\\M0::Calculate distance between lat/long pair", /Q, LTlatLongDistance()
//			help = {"Calculate straight-line distance between latitude/longitude pairs using Harversine formula"}
//		"\\M0:(:Create KML paths for Google Earth", /Q
//			help = {"It has been done before but not generalized (EVENTUALLY)"}
//	End
//	Submenu "Statistics"
//		"\\M0::Interval statistics", /Q, Interval_statistics()
//			help = {"Calculate basic statistics over specified intervals for wave(s)"}
//		"-"
//		"\\M0:(:Running mean", /Q, beep
//		"\\M0:!*:Box mean", /Q, lar_boxMeanPanel()
//		"\\M0:!*:Create daily box averages", /Q, boxDailyAvgByMin()
//		"\\M0:!*:Soft filter (HaPe Schmidt)", /Q, lar_softSpikeFilterPanel()
//			help = {"Apply soft spike filter to waves following method of HaPe Schmidt"}
//		"\\M0:!*:Apply soft spike filter", /Q, softSpikeFilter_tb()
//			help = {"Apply soft spike filter to waves following method of HaPe Schmidt"}
//		"\\M0:!*:Hard filter / NAN fill", /Q, lar_nanFillPanel()
//			help = {"Fill wave with NANs over a specified range by point indices or numeric comparison"}
//		"\\M0:!*:Set values to NAN using boundaries", convertValuesToNANs_tb()
//		"\\M0:!*:Fill a range with NANs by index", rangeFillWithNANs_tb()
//	End
//	Submenu "Utilities"
//		"\\M0:!*:Concat across folders", /Q, lar_concatDFsPanel()
//			help = {"Merge waves of the same name from mutiple datafolders"}
//		"\\M0:(:Lift files up from out of subfolders", /Q, beep
//			help = {"This would be helpful for Picarro G2301 files. A batch file is available - contact author. (EVENTUALLY)"}
//	End
//	"-"
//	"Windows Calculator", ExecuteScriptText "calc"
//End



//=====================================================================================
//					STATIC STRUCTURES  
//=====================================================================================

// Structure definition combines CheckBox & SetVariable into single entity accessible in prefs structures
Static Structure CBSV
	char 	checked					// whether or not checkbox is active
	double 	numVal					// numeric value of set variable
	uchar 	strVal[MAXCMDLEN] 		// string value of set variable
	char		isString					// boolean = true if SV holds string, not num
EndStructure




//=====================================================================================
//					(INTENDED TO BE) STATIC FUNCTIONS  
//=====================================================================================

// switches current datafolder to the full folder specified in popup menu
//	useful for popup menus populated with getFolderList()
Function puFolderSwitch(pa) : PopupMenuControl 
	STRUCT WMPopupAction &pa 			// popup menu affected
	switch(pa.eventCode) 						// what happened?
		case 2: 									// mouse up
			If ( DataFolderExists(pa.popStr) )			// if newly selected path exists
				SetDataFolder $pa.popStr					// switch to
				return 0 									// quit success
			else 									// and if not
				return -1 								// quit failure
			endif
	endswitch
End


// returns semicolon separated list of options available for SortList operation
Function/S puSortOptionList()  								// popup#   value
	string sortstring = "By creation date;"							// 	1		-1
	sortstring += "Ascending case-sensitive alphabetical ASCII;"		// 	2		0
	sortstring += "Ascending case-insensitive alphabetical ASCII;"		// 	3		4
	sortstring += "Ascending case-sensitive alphanumeric/system;"	// 	4		8
	sortstring += "Ascending case-insensitive alphanumeric/smart;"	//	5		16
	sortstring += "Ascending numeric;"								// 	6		2
	sortstring += "Descending case-sensitive alphabetical ASCII;"		// 	7		1
	sortstring += "Descending case-insensitive alphabetical ASCII;"	// 	8		5
	sortstring += "Descending case-sensitive alphanumeric/system;"	// 	9		9
	sortstring += "Descending case-insensitive alphanumeric/smart;"	//	10		17
	sortstring += "Descending numeric;"							// 	11		3
	return sortstring
End


// returns value associated with selected option from puSortOptionList() for passing to SortList
Function puSortOptionValue( puNum )
	variable puNum 		// popup value (1-indexed)
	Make/FREE/N=(12) sortOpt = { NAN, -1, 0, 4, 8, 16, 2, 1, 5, 9, 17, 3 }
	return sortOpt( puNum )
End


// used by the graph marquee context menu 
// initially part of the "Set Values in Marquee to NaN" snippet by JimProuty via IgorExchange.com
Function/S SetMarqueeToNanMenu( bounds )
	variable bounds
	return SelectString(bounds>1, "all Y traces;all X and Y traces;all X traces;", "all Y traces;")+TraceNameList("",";",1+4)
End


// accessed via the graph marquee context menu
//
// 2011.11.01 	finished revising, adding functionality; renamed function
// initially part of the "Set Values in Marquee to NaN" snippet by JimProuty via IgorExchange.com 
Function SetMarqueeToNaN( bounds )
	variable bounds 	// 	0:	use all marquee edges
					// 	1:	use left & right marquee edges
					// 	2: 	use top and bottom marquee edges
	string traceList, graphName, traceName, traceInfoo, xAxis, yAxis
	variable Xonly, Xalso, i, xMin, xMax, yMin, yMax
	
	graphName = WinName(0, 1)
	GetLastUserMenuInfo
	strswitch( S_value )
		case "all X traces":
			Xonly = 1
		case "all X and Y traces":
			Xalso = 1
		case "all Y traces":
			traceList = TraceNameList(graphName, ";", 1)
			break
		default:
			traceList = S_value
	endswitch
	
	for (i=0; i<ItemsInList(traceList); i+=1)
		traceName = StringFromList(i, traceList)
		wave/Z ywave = TraceNameToWaveRef(graphName,traceName)
		If ( !WaveExists(ywave) )
			print "Could not locate wave <"+NameOfWave(ywave)+"> - skipping"
			continue
		endif
		wave/Z xwave = XWaveRefFromTrace(graphName,traceName)
		traceInfoo = TraceInfo(graphName, traceName, 0)
		xAxis = StringByKey("XAXIS", traceInfoo)
		yAxis = StringByKey("YAXIS", traceInfoo)
		GetMarquee/W=$graphName $xAxis, $yAxis
		xMin= min(V_right, V_left)
		xMax= max(V_right, V_left)
		yMin= min(V_top, V_bottom)
		yMax= max(V_top, V_bottom)
		
		Duplicate/O/FREE ywave, mask
		switch ( bounds )
			case 0:			// all marquee boundaries
				If ( WaveExists(xwave) )
					mask = (ywave[p] > yMin) && (ywave[p] < yMax) && (xwave[p] > xMin) && (xwave[p] < xMax) ? NAN : 1
				else
					mask = (ywave[p] > yMin) && (ywave[p] < yMax) && (pnt2x(ywave,p) > xMin) && (pnt2x(ywave,p) < xMax) ? NAN : 1
				endif
				break
			case 1: 			// only use left/right edges
				If ( WaveExists(xwave) )
					mask = (xwave[p] > xMin) && (xwave[p] < xMax) ? NAN : 1
				else
					mask = (pnt2x(ywave,p) > xMin) && (pnt2x(ywave,p) < xMax) ? NAN : 1
				endif
				break
			case 2: 			// only use top/bottom edges
				mask = (ywave[p] > yMin) && (ywave[p] < yMax) ? NAN : 1
				break
		endswitch
		If ( !Xonly )
			ywave *= mask
		endif
		If ( Xalso && WaveExists(xwave) )
			xwave *= mask
		endif
	 endfor
	 return 0
End





//=====================================================================================
//					GRAPHICAL USER INTERFACES  
//=====================================================================================

////////////////////////////////		ListFilesIn_Panel  	\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
// 2011.10.27 	initial working version
StrConstant ksListFilesInPanelPrefsLoc = "root:Packages:ART:ListFilesInPanel"
StrConstant ksListFilesInWinName = "ListFilesInPanel"

// creates GUI panel for ListFilesIn(...) and returns names of panel window
Function/S ListFilesIn_Panel() : Panel
	string win = ksListFilesInWinName
	
	DoWindow/K $win
	DFREF sav0 = GetDataFolderDFR()
	DFREF prefs = NewDataFolderX(ksListFilesInPanelPrefsLoc)
	SetDataFolder prefs
	string path			= StrVarOrDefault(":gsPath", "**No directory specified yet**")
	string pathName 		= StrVarOrDefault(":gsPathName", "")
	string fileExt 			= StrVarOrDefault(":gsFileExt", "????")
	string fileFilter 		= StrVarOrDefault(":gsFileFilter", "")
	variable recurse 		= NumVarOrDefault(":gvRecurse", -1)
	variable recSet		= NumVarOrDefault(":gvRecSet", 0)
	variable sortPick 		= NumVarOrDefault(":gvSortPick", 5)
	variable sortBy		= NumVarOrDefault(":gvSortBy", puSortOptionValue(sortPick))
	variable wcW			= NumVarOrDefault(":gvWCW", 411)
	variable wcH 			= NumVarOrDefault(":gvWCH", 426)
	variable wcL 			= NumVarOrDefault(":gvWCL", 395)
	variable wcT 			= NumVarOrDefault(":gvWCT", 162)
	WAVE/T/Z fileList		= fileList
	WAVE/Z fileSel		= fileSel
	
	string/G gsPath		= path
	string/G gsPathName = pathName
	string/G gsFileExt 	= fileExt
	string/G gsFileFilter 	= fileFilter
	variable/G gvRecurse	= recurse
	variable/G gvRecSet	= recSet
	variable/G gvSortPick 	= sortPick
	variable/G gvSortBy 	= sortBy
	If ( !WaveExists(fileList) || !WaveExists(fileSel) )
		Make/O/N=0/T fileList
		Make/O/N=0 fileSel
	endif
	
	NewPanel/W=(wcL, wcT, wcL+wcW, wcT+wcH)/N=$win/K=1 as "File Search Dialog"
	win = S_name
	SetWindow $win, hook=ListFilesIn_HookProc
	
	Button btnSelectDir,pos={5,5},size={55,40},title="Select \rDirectory"
	Button btnSelectDir,help={"Choose the base directory to search for files in"}
	Button btnSelectDir,proc= ListFilesIn_BtnProc
	
	PathInfo $gsPathName
	If ( !V_flag )
		gsPath = "**No directory specified or could not be located**"
	else
		gsPath = S_path
	endif
	TitleBox titlePath,pos={65,5},size={340,42},fixedSize=1
	TitleBox titlePath,help={"Files will be searched for starting in this directory"}
	TitleBox titlePath,variable= gsPath

	SetVariable svFileFilter,pos={5,53},size={167,16},bodyWidth=90,title="File name filter: "
	SetVariable svFileFilter,help={"Enter a Grep expression to search for file names with or leave empty for all files"}
	SetVariable svFileFilter,value= gsFileFilter
	SetVariable svFileFilter,proc=ListFilesIn_SVProc
	
	Button btnFileFilterHelp,pos={175,51},size={20,20},title="?"
	Button btnFileFilterHelp,help={"Get help with file name filter"}
	Button btnFileFilterHelp,proc=ListFilesIn_BtnProc

	SetVariable svFileExt,pos={15,76},size={108,16},bodyWidth=40,title="File ext. filter: "
	SetVariable svFileExt,help={"Four-character extension code or extension beginning with \".\""}
	SetVariable svFileExt,value= gsFileExt
	SetVariable svFileExt,proc=ListFilesIn_SVProc
	
	Button btnFileExtHelp,pos={126,74},size={20,20},title="?"
	Button btnFileExtHelp,help={"Get help with file extension filter"}
	Button btnFileExtHelp,proc=ListFilesIn_BtnProc
	
	PopupMenu puSortBy,pos={5,100},size={323,21},bodyWidth=260,mode=1,title="Sort files by: "
	PopupMenu puSortBy,value= #"puSortOptionList()"
	PopupMenu puSortBy,mode=gvSortPick
	PopupMenu puSortBy,proc=ListFilesIn_PUProc
	
	TitleBox titleRecurse,pos={205,55},size={86,13},frame=0,title="Search recursion: "
	
	CheckBox chkRecurseAll,pos={205,74},size={80,14},mode=1,title="All subfolders"
	CheckBox chkRecurseAll,help={"Select to search for files in all subfolders"}
	CheckBox chkRecurseAll,value= (gvRecurse < 0)
	CheckBox chkRecurseAll,proc=ListFilesIn_CBProc
	
	CheckBox chkRecurseSet,pos={295,74},size={67,14},mode=1,title="Set depth:"
	CheckBox chkRecurseSet,help={"Select to search for files to a defined subfolder depth"}
	CheckBox chkRecurseSet,value= (gvRecurse >= 0)	
	CheckBox chkRecurseSet,proc=ListFilesIn_CBProc
	
	SetVariable svRecurse,pos={366,73},size={40,16},bodyWidth=40,limits={0,Inf,1},title=" "
	SetVariable svRecurse,help={"Set maximum subfolder search depth"}
	SetVariable svRecurse,value= gvRecSet
	SetVariable svRecurse,disable= ( gvRecurse < 0 ? 2 : 0 )
	SetVariable svRecurse,proc=ListFilesIn_SVProc
	
	ListBox lbFileList,pos={5,130},size={400,257},widths={1000},mode= 9,userColumnResize=1
	ListBox lbFileList,listWave= fileList
	ListBox lbFileList,selWave= fileSel
	Listbox lbFileList,proc=ListFilesIn_LBProc
	
	Button btnRefresh,pos={347,98},size={60,25},title="Refresh"
	Button btnRefresh,help={"Click to refresh the file search"}
	Button btnRefresh,proc=ListFilesIn_BtnProc
	
	Button btnRemove,pos={5,395},size={150,25},title="Remove selection from list"
	Button btnRemove,help={"Click to remove selected files from the list"}
	Button btnRemove,proc=ListFilesIn_BtnProc
	
	Button btnCancel,pos={270,395},size={50,25},title="Cancel"
	Button btnCancel,help={"Click to close without returning any selected file names"}
	Button btnCancel,proc=ListFilesIn_BtnProc
	
	Button btnContinue,pos={325,395},size={80,25},title="Continue >>"
	Button btnContinue,help={"Click to continue with selected file names"}
	Button btnContinue,proc=ListFilesIn_BtnProc
	
	SetDataFolder sav0
	return win
End

Function ListFilesIn_HookProc(whs) 
	STRUCT WMWinHookStruct &whs
	
	switch ( whs.eventCode )
		case 6:		// resize
		case 12:		// moved
			GetWindow $whs.WinName wsize
			variable scale = ScreenResolution / 72
			print "saving window coordinates"
			break
		case 0:		// activate
		case 1:		// deactivate
		case 2:		// kill
		case 3:		// mousedown
		case 4:		// mousemoved
		case 5:		// mouseup
		case 7:		// cursormoved
		case 8:		// modified (graph, notebook only)
		case 9:		// enablemenu
		case 10:		// menu
		case 11:		// keyboard
		case 13: 	// renamed
		case 14:		// subwindowKill
		case 15:		// hide
		case 16: 	// show
		case 17: 	// killVote
		case 18: 	// showTools
		case 19: 	// hideTools
		case 20:		// showInfo
		case 21: 	// hideInfo
		case 22: 	// mouseWheel
		case 23:		// spinUpdate (progress windows only)
			break
	endswitch
	return 0
End

Function ListFilesIn_BtnProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	DFREF prefs = NewDataFolderX(ksListFilesInPanelPrefsLoc)
	try
		SVAR gsPath		= prefs:gsPath
		SVAR gsPathName 	= prefs:gsPathName
		SVAR gsFileExt 		= prefs:gsFileExt
		SVAR gsFileFilter 	= prefs:gsFileFilter
		NVAR gvRecurse 	= prefs:gvRecurse
		NVAR gvRecSet		= prefs:gvRecSet
		NVAR gvSortPick 	= prefs:gvSortPick
		NVAR gvSortBy		= prefs:gvSortBy
		WAVE/T fileList		= prefs:fileList
		WAVE fileSel		= prefs:fileSel
	catch
		print "ListFilesInBtnProc: Error accessing preferences for ListFileInPanel"
		return -1
	endtry
	variable i
	
	switch( ba.eventCode )
		case 2: // mouse up
			strswitch( ba.ctrlName )
				case "btnCancel":
					string/G S_fileName = ""
					DoWindow/K $ba.win
					break
				case "btnContinue":
					string/G S_fileName = ""
					for (i=0; i<numpnts(fileList); i+=1)
						S_filename += fileList[i]+";"
					endfor
					DoWindow/K $ba.win
					break
				case "btnFileExtHelp":
					DisplayHelpTopic "IndexedFile"
					break
				case "btnFileFilterHelp":
					DisplayHelpTopic "Regular Expressions[Basic Regular Expressions]"
					break
				case "btnRemove":
					for (i=0; i<numpnts(fileSel); i+=1)
						If ( fileSel[i] )
							DeletePoints i, 1, fileList, fileSel
							i -= 1
						endif
					endfor
					If ( !numpnts(fileList) )
						Button btnContinue,win=$ba.win,disable=2
					endif
					break
				case "btnSelectDir":
					string tmpPN = "TmpPath_"+ba.win
					NewPath/O/Q/M="Select a base directory" $tmpPN
					If ( V_flag ) // user cancelled
						KillPath/Z tmpPN
						return -1
					endif
					PathInfo $tmpPN
					If ( !V_flag )
						gsPath = "**Directory could not be found**"
						KillPath/Z tmpPN
						return -1
					endif
					gsPathName = tmpPN
					gsPath = S_path
					// NO BREAKS //
				case "btnRefresh":
					Redimension/N=0 prefs:fileList, prefs:fileSel; DoUpdate
					string filesFound = ListFilesIn( gsPathName, gsFileFilter, gsFileExt, gvRecurse, gvSortBy )
					variable found = ItemsInList(filesFound)
					If ( found )
						Make/O/T/N=(found) prefs:fileList
						Make/O/N=(found) prefs:fileSel = 0
						for (i=0; i<found; i+=1)
							fileList[i]= StringFromList(i, filesFound)
						endfor
						Button btnContinue,win=$ba.win,disable=0
					else
						Button btnContinue,win=$ba.win,disable=2
					endif
					break
			endswitch
			break
		case -1: // control being killed
		case 1: // mouse down
		case 2: // mouse up
		case 3: // mouse up outside control
		case 4: // mouse moved
		case 5: // mouse enter
		case 6: // mouse leave
			break
	endswitch
	return 0
End

Function ListFilesIn_CBProc(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba
	
	DFREF prefs = NewDataFolderX(ksListFilesInPanelPrefsLoc)
	try
		NVAR gvRecurse = prefs:gvRecurse
		NVAR gvRecSet = prefs:gvRecSet
	catch
		print "ListFilesInChkProc: Error accessing preferences for ListFilesInPanel"
		return -1
	endtry
	switch ( cba.eventCode )
		case 2: // mouse up
			strswitch( cba.ctrlName )
				case "chkRecurseAll":
					gvRecurse = -1
					break
				case "chkRecurseSet":
					gvRecurse = gvRecSet
					break
			endswitch
			CheckBox chkRecurseAll,win=$cba.win,value= (gvRecurse < 0)
			CheckBox chkRecurseSet,win=$cba.win,value= (gvRecurse >= 0)
			SetVariable svRecurse,win=$cba.win,disable= ( gvRecurse < 0 ? 2 : 0 )
			break
		case -1: // control being killed
			break
	endswitch
	return 0
End

Function ListFilesIn_LBProc(lba) : ListBoxControl
	STRUCT WMListboxAction &lba

	switch( lba.eventCode )
		case 3: // double click
			DeletePoints lba.row, 1, lba.listWave, lba.selWave
			If ( !numpnts( lba.listWave ) )
				Button btnContinue,win=$lba.win,disable=2
			endif
			break
		case -1: // control being killed
		case 1: // mouse down
		case 2: // mouse up
		case 4: // cell selection (mouse or arrow keys)
		case 5: // cell selection plus shift key
		case 6: // begin edit
		case 7: // finish edit
		case 8: // vertical scroll
		case 9: // horizontal scroll
		case 10: // top row set or first column set
		case 11: // column divider resized
		case 12: // keystroke, char code is in row field
		case 13: // checkbox clicked (Igor 6.2 or later)
			break
	endswitch
	return 0
End

Function ListFilesIn_PUProc(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	DFREF prefs = NewDataFolderX(ksListFilesInPanelPrefsLoc)
	try
		NVAR gvSortPick = prefs:gvSortPick
		NVAR gvSortBy = prefs:gvSortBy
	catch
		print "ListFilesInPUProc: Error accessing preferences for ListFilesInPanel"
		return -1
	endtry
	switch( pa.eventCode )
		case 2: // mouse up
			strswitch( pa.ctrlName )
				case "puSortBy":
					gvSortPick = pa.popNum
					gvSortBy = puSortOptionValue( pa.popNum )
					break
			endswitch
			break
		case -1: // control being killed
			break
	endswitch

	return 0
End

Function ListFilesIn_SVProc(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	DFREF prefs = NewDataFolderX(ksListFilesInPanelPrefsLoc)
	try
		SVAR gsFileExt = prefs:gsFileExt
	catch
		print "ListFilesInSVProc: Error accessing preferences for ListFilesInPanel"
		return -1
	endtry
	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			strswitch( sva.ctrlName )
				case "svFileExt":
					string sval = sva.sval
					If ( cmpstr(sval[0],".") && (strlen(sval) != 4) )
						SetVariable svFileExt,win=$sva.win,valueColor=(65535,65535,65535),valueBackColor=(65280,0,0)
						Button btnContinue,win=$sva.win,disable=2
					else
						SetVariable svFileExt,win=$sva.win,valueColor=(0,0,0),valueBackColor=(65535,65535,65535)	
						Button btnContinue,win=$sva.win,disable=0
					endif
					break
				case "svFileFilter":
					// no action
					break
			endswitch
			break
		case -1: // control being killed
		case 4: // scroll wheel up if increment = 0
		case 5: // scroll wheel down if increment = 0
		case 6: // value changed by dependency
			break
	endswitch
	return 0
End



////////////////////////////////		LoadLGR_Panel  		\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
// 2011.10.28 	initial working version
StrConstant ksLoadLGRPanelPrefsLoc = "root:Packages:ART:LoadLGRPanel"
StrConstant ksLoadLGRWinName = "LoadLGRPanel0"

// builds GUI panel for LoadLGR(...) and returns name of panel
Function/S LoadLGR_Panel()
	string win = ksLoadLGRWinName
	
	DoWindow/K $win
	DFREF sav0 = GetDataFolderDFR()
	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	SetDataFolder prefs
	string model 			= StrVarOrDefault(":gsModel", "")
	variable overwrite 		= NumVarOrDefault(":gvOverwrite", 0)
	variable resamp 		= NumVarOrDefault(":gvResamp", 0)
	variable concat 		= NumVarOrDefault(":gvConcat", 0)
	variable Bpick		= NumVarOrDefault(":gvBpick", 1)
	variable newRate 		= NumVarOrDefault(":gvNewRate", 0.1)
	variable subPick 		= NumVarOrDefault(":gvSubPick", 1)
	variable forceSub 		= NumVarOrDefault(":gvForceSub", 0)
	variable unique 		= NumVarOrDefault(":gvUnique", 1)
	variable skip			= NumVarOrDefault(":gvSkip", 0)
	WAVE/T/Z fileList 	= fileList
	WAVe/Z	fileSel 		= fileSel
	
	string/G gsModel 		= model
	variable/G gvOverwrite = overwrite
	variable/G gvResamp 	= resamp
	variable/G gvConcat 	= concat
	variable/G gvBpick 	= Bpick
	variable/G gvNewRate 	= newRate
	variable/G gvSubPick 	= subPick
	variable/G gvForceSub = forceSub
	variable/G gvUnique 	= unique
	variable/G gvSkip 		= skip
	If ( !WaveExists(fileList) || !WaveExists(fileSel) )
		Make/O/N=0/T fileList
		Make/O/N=0 fileSel
	endif
	
	NewPanel/W=(404,127,765,662)/N=$win/K=1 as "LGR File Loader"
	win = S_name
	SetWindow $win, hook(default)=LoadLGR_HookProc
	
	TitleBox title pos={10,5},frame=0,title="\\Z14File Loader for Los Gatos Research Analzyers"
	
	GroupBox grpInput,pos={5,31},size={350,262},title="Source Files"
	
	Button btnSelFile,pos={13,49},size={80,25},title="Select file(s)"
	Button btnSelFile,help={"Replace the list with a selection of files"}
	Button btnSelFile,proc=LoadLGR_BtnProc
	
	Button btnAddFile,pos={97,49},size={70,25},title="Add file(s)"
	Button btnAddFile,help={"Add files to the list before the selected row or at the end if no selection"}
	Button btnAddFile,proc=LoadLGR_BtnProc
	
	Button btnSelDir,pos={237,49},size={110,25},title="Search in directory..."
	Button btnSelDir,help={"Replace the list with files found in certain directories"}
	Button btnSelDir,proc=LoadLGR_BtnProc
	
	ListBox lbFileList,pos={13,80},size={334,175},widths={1000,0},userColumnResize=1,mode= 9
	ListBox lbFileList,help={"Files listed here will be loaded in the order shown"}
	ListBox lbFileList,listWave= fileList
	ListBox lbFileList,selWave= fileSel
	ListBox lbFileList,proc=LoadLGR_LBProc

	Button btnRemove,pos={13,260},size={100,25},title="Remove selection"
	Button btnRemove,help={"Remove selection from the list"}
	Button btnRemove,proc=LoadLGR_BtnProc

	Button btnClear,pos={118,261},size={60,25},title="Clear list"
	Button btnClear,help={"Clear all files from the list"}
	Button btnClear,proc=LoadLGR_BtnProc

	Button btnUp,pos={317,261},size={30,25},title="UP"
	Button btnUp,help={"Move selected row upward in list"}
	Button btnUp,proc=LoadLGR_BtnProc

	Button btnDown,pos={284,261},size={30,25},title="DN"
	Button btnDown,help={"Move selected row downward in list"}
	Button btnDown,proc=LoadLGR_BtnProc

	GroupBox grpOptions,pos={5,298},size={349,203},title="Options"

	PopupMenu puModel,pos={17,317},size={330,21},bodyWidth=250,title="Analyzer model: "
	PopupMenu puModel,help={"Select the gas analzyer model which created the data files"}
	PopupMenu puModel,value= #"LoadLGR_puModelList()"
	PopupMenu puModel,popvalue= gsModel
	PopupMenu puModel,proc=LoadLGR_PUProc

	PopupMenu puBstring,pos={23,344},size={324,21},bodyWidth=250,title="Data columns: "
	PopupMenu puBstring,help={"Select which data columns to load"}
	PopupMenu puBstring,value= #("LoadLGR_puBoptions(\""+gsModel+"\")")
	PopupMenu puBstring,mode= gvBpick
	PopupMenu puBstring,proc=LoadLGR_PUProc

	CheckBox cbResamp,pos={13,374},size={114,14},title="Resample waves to "
	CheckBox cbResamp,help={"Select to resample data files at a constant rate"}
	CheckBox cbResamp,variable= gvResamp
	CheckBox cbResamp,proc=LoadLGR_CBProc

	SetVariable svNewRate,pos={130,374},size={40,16},bodyWidth=40,title=" ",limits={0,100,0.05},live= 1
	SetVariable svNewRate,value= gvNewRate
	SetVariable svNewRate,proc=LoadLGR_SVProc

	TitleBox titleHz,pos={173,375},size={13,13},title="Hz",frame=0

	CheckBox cbConcat,pos={232,374},size={103,14},title="Concatenate files "
	CheckBox cbConcat,help={"Select to concatenate all the data files in the order listed"}
	CheckBox cbConcat,variable= gvConcat
	CheckBox cbConcat,proc=LoadLGR_CBProc

	CheckBox cbForceSub,pos={13,395},size={227,14},title="Use subfolders even when loading single file"
	CheckBox cbForceSub,help={"Select to load individual file in subfolder"}
	CheckBox cbForceSub,variable= gvForceSub
	CheckBox cbForceSub,proc=LoadLGR_CBProc

	PopupMenu puSub,pos={14,416},size={316,21},bodyWidth=200,title="Name subfolders using: "
	PopupMenu puSub,help={"Select how subfolders are generated when loading multiple files"}
	PopupMenu puSub,value= #"\"name of file;name of file's parent folder;masked string;\""
	PopupMenu puSub,mode= gvSubPick
	PopupMenu puSub,proc=LoadLGR_PUProc

	TitleBox titleOverwrite,pos={13,463},size={162,13},title="Subfolder & wave overwrite policy:"
	TitleBox titleOverwrite,frame=0

	CheckBox cbUnique,pos={190,448},size={113,14},title="Create unique name",mode=1
	CheckBox cbUnique,variable= gvUnique
	CheckBox cbUnique,proc=LoadLGR_CBProc

	CheckBox cbOverwrite,pos={190,464},size={101,14},title="Overwrite existing",mode=1
	CheckBox cbOverwrite,help={"Select to overwrite existing waves with conflicting names"}
	CheckBox cbOverwrite,variable= gvOverwrite
	CheckBox cbOverwrite,proc=LoadLGR_CBProc

	CheckBox cbSkip,pos={190,480},size={85,14},title="Skip if existing",mode=1
	CheckBox cbSkip,variable= gvSkip
	CheckBox cbSkip,proc=LoadLGR_CBProc

	Button btnCancel,pos={6,505},size={50,25},title="Cancel"
	Button btnCancel,help={"Cancel without loading any files"}
	Button btnCancel,proc=LoadLGR_BtnProc

	Button btnLoad,pos={275,505},size={80,25},title="Load Files >> "
	Button btnLoad,help={"Load files in the list"}
	Button btnLoad,proc=LoadLGR_BtnProc
	
	SetDataFolder sav0
	return win
End

Function/S LoadLGR_puModelList()
	string list = "Carbon Dioxide Analyzer;"
	list += "Carbon Dioxide Isotope Analyzer;"
	list += "Carbon Monoxide Analyzer;"
	list += "Economical Ammonia Analyzer;"
	list += "Fast Methane Analyzer;"
	list += "Greenhouse Gas Analyzer (CH4, CO2, H2O);"
	list += "Hydrogen Flouride (HF) Analyzer;"
	list += "Isotopic N2O Analyzer;"
	list += "Isotopic Water Analyzer (Liquid+Vapor) - Enhanced Performance;"
	list += "Liquid Water Isotope Analyzer;"
	list += "Methane Carbon Isotope Analyzer;"
	list += "N2O/CO Analyzer;"
	list += "NO2 Analyzer (nitrogen dioxide);"
	list += "Water-Vapor Isotope Analyzer;"
	return list
End

Function/S LoadLGR_puBoptions( modelName )
	string modelName
	string list = ""
	strswitch (modelName)
		case "Carbon Dioxide Analyzer":
		case "Carbon Dioxide Isotope Analyzer":
		case "Carbon Monoxide Analyzer":
		case "Economical Ammonia Analyzer":
		case "Fast Methane Analyzer":
		case "Greenhouse Gas Analyzer (CH4, CO2, H2O)":
		case "Hydrogen Flouride (HF) Analyzer":
		case "Isotopic N2O Analyzer":
		case "Isotopic Water Analyzer (Liquid+Vapor) - Enhanced Performance":
		case "Liquid Water Isotope Analyzer":
		case "Methane Carbon Isotope Analyzer":
			return "**Not implemented**"
			break
		case "N2O/CO Analyzer":
			list += "All columns;"
			list += "CO/N2O/H2O [amb+dry];"
			list += "CO/N2O/H2O [ambient];"
			list += "CO/N2O/H2O [dry];"
			list += "CO/N2O [amb+dry];"
			list += "CO/N2O [ambient];"
			list += "CO/N2O [dry];"
			list += "CO/H2O [amb+dry];"
			list += "CO/H2O [ambient];"
			list += "CO/H2O [dry];"
			list += "N2O/H2O [amb+dry];"
			list += "N2O/H2O [ambient];"
			list += "N2O/H2O [dry];"
			return list
			break
		case "NO2 Analyzer (nitrogen dioxide)":
		case "Water-Vapor Isotope Analyzer":
			return "**Not implemented**"
			break
		default:
			return ""
	endswitch
End

Function/S LoadLGR_puBstring( modelName, Boption )
	string modelName
	variable Boption
	string B = ""
	strswitch (modelName)
		case "Carbon Dioxide Analyzer":
		case "Carbon Dioxide Isotope Analyzer":
		case "Carbon Monoxide Analyzer":
		case "Economical Ammonia Analyzer":
		case "Fast Methane Analyzer":
		case "Greenhouse Gas Analyzer (CH4, CO2, H2O)":
		case "Hydrogen Flouride (HF) Analyzer":
		case "Isotopic N2O Analyzer":
		case "Isotopic Water Analyzer (Liquid+Vapor) - Enhanced Performance":
		case "Liquid Water Isotope Analyzer":
		case "Methane Carbon Isotope Analyzer":
			break
		case "N2O/CO Analyzer":
			B += "C=1, F=8, N=timestamp, T=4;"
			switch( Boption )
				case 0:		// all columns
					B += ""
					break
				case 1:		// all gases, ambient + dry
					B += "N=CO_ppm; N=CO_ppm_SE; N=N2O_ppm; N=N2O_ppm_SE;"
					B += "N=H2O_ppm; N=H2O_ppm_SE;"
					break
					
//			B += "N=co_ppm; N=co_ppm_se; N=n2o_ppm; N=n2o_ppm_se; N=h2o_ppm; "
//			B += "N=h2o_ppm_se; N=GasP_torr; N=GasP_torr_se; N=GasT_C; N=GasT_C_se; N=AmbT_C; "
//			B += "N=AmbT_C_se; N=Gnd; N=Gnd_se; N=LTCO_v; N=LTCO_v_se; N=EtSt0; N=EtSt0_se; "
//			B += "N=AIN5; N=AIN5_se; N=AIN6; N=AIN6_se; N=AIN7; N=AIN7_se; N=Fit_Flag; "
//			break
//		case 1: 		// CO/N2O/H2O conc. 
//			B += "N=co_ppm; N='_skip_'; N=n2o_ppm; N='_skip_'; N=h2o_ppm; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 2: 		// CO/N2O/H2O conc. & SE
//			B += "N=co_ppm; N=co_ppm_se; N=n2o_ppm; N=n2o_ppm_se; N=h2o_ppm; "
//			B += "N=h2o_ppm_se; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 3: 		// CO/N2O conc. 
//			B += "N=co_ppm; N='_skip_'; N=n2o_ppm; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 4: 		// CO/N2O conc. & SE
//			B += "N=co_ppm; N=co_ppm_se; N=n2o_ppm; N=n2o_ppm_se; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 5: 		// CO/H2O conc. 
//			B += "N=co_ppm; N='_skip_'; N='_skip_'; N='_skip_'; N=h2o_ppm; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 6: 		// CO/H2O conc. & SE
//			B += "N=co_ppm; N=co_ppm_se; N='_skip_'; N='_skip_'; N=h2o_ppm; "
//			B += "N=h2o_ppm_se; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 7: 		// N2O/H2O conc. 
//			B += "N='_skip_'; N='_skip_'; N=n2o_ppm; N='_skip_'; N=h2o_ppm; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		case 8: 		// N2O/H2O conc. & SE
//			B += "N='_skip_'; N='_skip_'; N=n2o_ppm; N=n2o_ppm_se; N=h2o_ppm; "
//			B += "N=h2o_ppm_se; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; " 
//			B += "N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; N='_skip_'; "		
//			break
//		default:
//			Abort "Invalid column selection flag was provided: "+num2str(cols)
	endswitch					

			break
		case "NO2 Analyzer (nitrogen dioxide)":
		case "Water-Vapor Isotope Analyzer":
		default:
	endswitch
	return B
End


Function LoadLGR_HookProc(whs)
	STRUCT WMWinHookStruct &whs
	
	switch ( whs.eventCode )
		case 6:		// resize
		case 12:		// moved
			GetWindow $whs.winName wsize
			variable scale = ScreenResolution / 72
			print "saving window coordinates"
			break
		case 0:		// activate
		case 1:		// deactivate
		case 2:		// kill
		case 3:		// mousedown
		case 4:		// mousemoved
		case 5:		// mouseup
		case 7:		// cursormoved
		case 8:		// modified (graph, notebook only)
		case 9:		// enablemenu
		case 10:		// menu
		case 11:		// keyboard
		case 13: 	// renamed
		case 14:		// subwindowKill
		case 15:		// hide
		case 16: 	// show
		case 17: 	// killVote
		case 18: 	// showTools
		case 19: 	// hideTools
		case 20:		// showInfo
		case 21: 	// hideInfo
		case 22: 	// mouseWheel
		case 23:		// spinUpdate (progress windows only)
			break
	endswitch
	return 0
End

Function LoadLGR_BtnProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	try
		WAVE/T fileList = prefs:fileList
		WAVE fileSel = prefs:fileSel
	catch
		print "funcname: Error accessing preferences"
		return -1
	endtry
	variable i, refnum, found
	string files
	
	switch( ba.eventCode )
		case 2: // mouse up
			strswitch( ba.ctrlName )
				case "btnAddFile":
					Open/D/R/MULT=1/F=ksFileFilter refnum
					files = ReplaceString(num2char(13), S_fileName, ";")
					found = ItemsInList(files)
					If ( found )
						FindValue/V=1 fileSel
						variable atpoint = (V_value < 0) ? numpnts(fileSel) : V_value
						InsertPoints atpoint, found, fileList, fileSel
						for (i=atpoint; i<(atpoint+found); i+=1)
							fileList[i] = StringFromList(i-atpoint, files)
						endfor
						Button btnLoad,win=$ba.win,disable=0
					endif
					break
				case "btnCancel":
					DoWindow/K $ba.win
					break
				case "btnClear":
					Redimension/N=0 fileList, fileSel
					Button btnAddFile,win=$ba.win,disable=0
					Button btnDown,win=$ba.win,disable=2
					Button btnUp,win=$ba.win,disable=2
					Button btnLoad,win=$ba.win,disable=2
					break
				case "btnDown":
					print "moving selection down"
					break
				case "btnLoad":
					print "Whoa - we're not quite ready yet"
					break
				case "btnRemove":
					for (i=0; i<numpnts(fileSel); i+=1)
						if ( fileSel[i] )
							DeletePoints i, 1, fileList, fileSel
							i -= 1
						endif
					endfor
					Button btnAddFile,win=$ba.win,disable=0
					Button btnDown,win=$ba.win,disable=2
					Button btnUp,win=$ba.win,disable=2
					if ( !numpnts(fileList) )
						Button btnLoad,win=$ba.win,disable=2
					endif
					break
				case "btnSelDir":
					string pname = ListFilesIn_Panel()
					PauseForUser $pname
					SVAR gS_fileName = :S_fileName
					found = ItemsInList(gS_fileName)
					If ( found )
						Make/O/T/N=(found) prefs:fileList
						Make/O/N=(found) prefs:fileSel
						for (i=0; i<found; i+=1)
							fileList[i] = StringFromList(i, gS_fileName)
						endfor
						Button btnLoad,win=$ba.win,disable=0
					else
						Button btnLoad,win=$ba.win,disable=2
					endif
					break
				case "btnSelFile":
					Open/D/R/MULT=1/F=ksFileFilter refnum
					files = ReplaceString(num2char(13), S_fileName, ";")
					found = ItemsInList(files)
					If ( found )
						Make/O/T/N=(found) prefs:fileList
						Make/O/N=(found) prefs:fileSel
						for (i=0; i<found; i+=1)
							fileList[i] = StringFromList(i, files)
						endfor
						Button btnLoad,win=$ba.win,disable=0
					else
						Button btnLoad,win=$ba.win,disable=2
					endif
					break
				case "btnUp":
					print "moving selection up"
					break
			endswitch
			break
		case -1: // control being killed
		case 1: // mouse down
		case 2: // mouse up
		case 3: // mouse up outside control
		case 4: // mouse moved
		case 5: // mouse enter
		case 6: // mouse leave
			break
	endswitch
	return 0
End

Function LoadLGR_CBProc(cba) : CheckBoxControl
	STRUCT WMCheckboxAction &cba

	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	try
		NVAR gvConcat 		= prefs:gvConcat
		NVAR gvForceSub 	= prefs:gvForceSub
		NVAR gvOverwrite 	= prefs:gvOverwrite
		NVAR gvResamp		= prefs:gvResamp
		NVAR gvSkip 		= prefs:gvSkip
		NVAR gvUnique		= prefs:gvUnique
	catch
		print "LoadLGR_CBProc: Error accessing preferences"
		return -1
	endtry
	switch( cba.eventCode )
		case 2: // mouse up
			strswitch( cba.ctrlName )
				case "cbConcat":
					gvConcat = cba.checked
					break
				case "cbForceSub":
					gvForceSub = cba.checked
					break
				case "cbOverwrite":
					gvOverwrite = cba.checked
					If ( gvOverwrite )
						gvSkip = 0
						gvUnique = 0
					endif
					break
				case "cbResamp":
					gvResamp = cba.checked
					break
				case "cbSkip":
					gvSkip = cba.checked
					If ( gvSkip )
						gvOverwrite = 0
						gvUnique = 0
					endif
					break
				case "cbUnique":
					gvUnique = cba.checked
					If ( gvUnique )
						gvOverwrite = 0
						gvSkip = 0
					endif
					break
			endswitch
			break
		case -1: // control being killed
			break
	endswitch
	return 0
End

Function LoadLGR_LBProc(lba) : ListBoxControl
	STRUCT WMListboxAction &lba
//	Variable row = lba.row
//	Variable col = lba.col
//	WAVE/T/Z listWave = lba.listWave
	WAVE/Z selWave = lba.selWave

	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	try
		// looking up vars
	catch
		print "funcname: Error accessing preferences"
		return -1
	endtry
	switch( lba.eventCode )
		case 3: // double click
			DeletePoints lba.row, 1, lba.listWave, lba.selWave
			If ( !numpnts( lba.listWave ) )
				Button btnContinue,win=$lba.win,disable=2
			endif
			break
		case -1: // control being killed
		case 1: // mouse down
		case 2: // mouse up
		case 4: // cell selection (mouse or arrow keys)
		case 5: // cell selection plus shift key
			variable selected = Sum(lba.selWave)
			If ( !selected )
				Button btnAddFile,win=$lba.win,disable=0
				Button btnDown,win=$lba.win,disable=2
				Button btnUp,win=$lba.win,disable=2
			elseif ( selected > 1 )
				Button btnAddFile,win=$lba.win,disable=2
				Button btnDown,win=$lba.win,disable=2
				Button btnUp,win=$lba.win,disable=2
			else
				Button btnAddFile,win=$lba.win,disable=0
				Button btnDown,win=$lba.win,disable=0
				Button btnUp,win=$lba.win,disable=0
			endif
			break
		case 6: // begin edit
		case 7: // finish edit
		case 8: // vertical scroll
		case 9: // horizontal scroll
		case 10: // top row set or first column set
		case 11: // column divider resized
		case 12: // keystroke, char code is in row field
		case 13: // checkbox clicked (Igor 6.2 or later)
	endswitch
	return 0
End

Function LoadLGR_PUProc(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	try
		SVAR gsModel 		= prefs:gsModel
		NVAR gvBpick 		= prefs:gvBpick
		NVAR gvSubPick 	= prefs:gvSubPick
	catch
		print "LoadLGR_PUProc: Error accessing preferences"
		return -1
	endtry
	switch( pa.eventCode )
		case 2: // mouse up
			strswitch( pa.ctrlName )
				case "puModel":
					gsModel = pa.popStr
					PopupMenu puBstring,win=$pa.win,value= #("LoadLGR_puBoptions(\""+gsModel+"\")")
					break
				case "puBstring":
					gvBpick = pa.popNum
					break
				case "puSub":
					gvSubPick = pa.popNum
					break
			endswitch
			break
		case -1: // control being killed
			break
	endswitch
	return 0
End

Function LoadLGR_SVProc(sva) : SetVariableControl
	STRUCT WMSetVariableAction &sva

	DFREF prefs = NewDataFolderX(ksLoadLGRPanelPrefsLoc)
	try
		// looking up vars
	catch
		print "LoadLGR_SVProc: Error accessing preferences"
		return -1
	endtry
	switch( sva.eventCode )
		case 1: // mouse up
		case 2: // Enter key
		case 3: // Live update
			Variable dval = sva.dval
			String sval = sva.sval
			break
		case -1: // control being killed
		case 4: // scroll wheel up if increment = 0
		case 5: // scroll wheel down if increment = 0
		case 6: // value changed by dependency
			break
	endswitch
	return 0
End




//===========================================================================\\
#EndIf									// DEVELOPMENT FLAG - MUST BE LAST LINE OF FILE  \\
//===========================================================================\\