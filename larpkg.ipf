//=====================================================================================
//	LAR Library
//	
//	Patrick O'Keeffe 					Laboratory for Atmospheric Research 
// 	pokeeffe@wsu.edu				Dept. of Civil & Environmental Engineering 
// 	509.335.7246 					Washington State University 
//
//	Licensed under the MIT License, available at http://opensource.org/licenses/MIT
//
//=====================================================================================
#pragma rtGlobals = 1				// Use modern global access method.
#pragma IgorVersion = 6.20
#pragma version = 0.20130313		// this field last changed on, YYYYMMDD

#If ( 1 )							// 1 to compile; 0 to disable procedure
//=====================================================================================
//					INCLUDES  
//=====================================================================================
#include <Remove Points>
//#include <New Polar Graphs>
//#include <Rose Plot>
//#include <WaveSelectorWidget> ??
//#include <Time-Frequency> ??


//====================================================================================
//					CONSTANTS  
//=====================================================================================
StrConstant ksFileFilter = "Plain-text data (.txt .dat .csv .log):.txt,.dat,.csv,.log;Archived data (.zip .7z .rar):.zip,.7z,.rar;All files (*.*):.*;"

// molecular weights
Constant kMW_dryair = 28.9645 		// g / mol			dry air
//Constant kMW_ch4 = 
Constant kMW_co2 = 44.01			// g / mol 			carbon dioxide
//Constant kMW_co = 
Constant kMW_h2o = 18.01528 		// g / mol			water
//Constant kMW_n2o = 


// gas constants
Constant kRu = 8.3144621e-2 			// (mb m^3) / (mol K)		universal gas constant
Constant kR_dryair = 2.87057e-3		// (mb m^3) / (g K)		specific gas constant for dry air
Constant kR_h2o = 4.61495e-3			// (mb m^3) / (g K) 		specific gas constant for water vapor


// specific heats
Constant kCp_dryair = 1.0057e-2 		// (mb m^3) / (g K)		specific heat capacity at constant pressure for dry air
//Constant kCpv = 1875	// ± 25 J / (kg K)		specific heat capacity at constant pressure for water vapor ????


// other constants
Constant kVonKarman = 0.4 				// dimensionless
Constant kGravity = 9.8					// m / s^2


// as defined for use in structures
Constant MAX_OBJ_NAME  = 31		// names
Constant MAX_WIN_PATH = 199		// windows
Constant MAX_UNITS = 49			// axis units
Constant MAXCMDLEN = 400		// string values


//=====================================================================================
//					PUBLIC FUNCTIONS 
//=====================================================================================


// returns <wrefs> with new references <addref> at point <point>
Function/WAVE AddWaveRef( addref, wrefs, beforePoint )
	wave addref			// reference to add to <wrefs>
	wave/WAVE wrefs	// wave of references to add to
	variable beforePoint	// inserted at this point, existing values moved backward
						//	0 = front		numpnts(wrefs) = back
	InsertPoints beforePoint, 1, wrefs
	wrefs[beforePoint] = addref
	return wrefs
end


// Returns wave of references to all data folders in current data folder (except Packages, if in root:)
//
// 2011.09		initial release
Function/WAVE AllDataFoldersHere(sortBy)
	variable sortBy
	string found = DataFolderDir(1)
	found = RemoveFromList("Packages", ReplaceString(",", found[8, strlen(found)-2], ";") )
	if ( sortBy+1 ) 	// true except for -1
		found = SortList( found, ";", sortBy)
	endif
	Make/FREE/DF/N=(ItemsInList(found)) reflist
	variable i
	for (i=0; i<ItemsInList(found); i+=1)
		reflist[i] = $StringFromList(i, found)
	endfor
	return reflist 	// not threadsafe
End


// Returns wave containing references to all waves in current data folder
//
// 2011.09		initial release
Function/WAVE AllWavesHere(sortBy)
	variable sortBy
	string found = WaveList("*", ";", "") 
	if ( sortBy+1 ) 	// true except for -1
		found = SortList( found, ";", sortBy )
	endif
	Make/FREE/WAVE/N=(ItemsInList(found)) refw
	variable i
	for (i=0; i<ItemsInList(found); i+=1)
		refw[i] = $StringFromList(i, found)
	endfor
	return refw	// not threadsafe
End


// returns ambient temp, Celcius, derived from sonic temp
//
// 2011.11.10 	now works in Celcius
// 2011.11.09 	written
ThreadSafe Function AmbientTemp( Ts, Q_ )
	variable Ts 			// abs. sonic temp.			Celcius
	variable Q_			// specific humidity 			dimensionless
	
	return ( (Ts+273.15) / (1+ 0.51*Q_) ) - 273.15
End


// writes all the waves in <wrefs> to a .csv for each interval of size <interval> in <tstamp>
//
// 2011.09		initial release
Function BaleWaves( tstamp, wrefs, interval, aligned, formatTableName, destNameMask, destPath, overwrite)
	wave/D tstamp			// time to bale according to
	wave/WAVE wrefs		// waves to affect, in desired order of output L->R
	variable interval			// bale interval in seconds
	variable aligned			// nonzero to start at some multiple of the interval
	string formatTableName	// name of existing table with desired formatting
	string destNameMask		// output file name format; use field codes from StringFromMaskedVar
	string destPath			// full path to output directory or "" for prompt
	variable overwrite			// nonzero to overwrite existing files, default: get prompted to save as...
	
	variable oi, wn, mgc, ints	// output index, wave num, modifygraph count, intervals
	string tmpTN, mglist, outname	// temp table name, modifygraph list, output file name
	
	DoWindow $formatTableName // not threadsafe
	If ( !V_flag )
		print ("BaleWaves: could not find table <"+formatTableName+"> - aborting")
		return -1
	endif
	
	If (strlen(destPath))
		NewPath/Q/O/M=("Select output directory") balewoutdir, destPath
	else
		NewPath/Q/O/M=("Select output directory") balewoutdir
	endif
	
	wave/D bounds = IntervalBoundaries( tstamp, interval, aligned) 
	ints = DimSize(bounds,0)
	DFREF savDFR = GetDataFolderDFR()
	SetDataFolder NewDataFolderX("root:Packages:art:BaleWavesTemp")
	
	mglist = ReplaceString(num2char(13),WinRecreation(formatTableName, 1),";")
	for (oi=0; oi<ints; oi+=1)
		Edit/HIDE=1
		tmpTN = S_name 	// returned by Edit 
		for (wn=0; wn<numpnts(wrefs); wn+=1)
			wave now = wrefs[wn]
			Duplicate/O/R=[ bounds[oi][%lo], bounds[oi][%hi] ] now, $NameOfWave(now)
			AppendToTable $NameOfWave(now)
		endfor

		// mglist = used to be here... haven't tested since moved outside loop! 
		for (mgc=3; mgc<(ItemsInList(mglist)-2); mgc+=1)
			Execute StringFromList(mgc, mglist)
		endfor

		outname = StringFromMaskedVar( destNameMask, tstamp[ bounds[oi][%lo] ] )
		If (overwrite)
			SaveTableCopy/O/W=$tmpTN/T=2/P=balewoutdir as outname
		else
			SaveTableCopy/W=$tmpTN/T=2/P=balewoutdir as outname
		endif
		
		If ( V_flag )
			DoWindow/K $tmpTN
			print "BaleWaves: Detected error condition or user cancel - aborting"
			break
		endif
		DoWindow/K $tmpTN
	endfor

	KillDataFolder :
	SetDataFolder savDFR
	printf "BaleWaves: finished writing %d files ", ints
End	


// Rounds value <inVal> to specified digits column <place> using bankers' rounding; ie: if the remainder
// is one-half (0.5), rounding proceeds towards the nearest even integer rather than upwards to avoid an  
// upward statistical bias over large data sets. 
//
// 2011.10.27 	*bug fix* added second abs() around statement compared to maxdev
// 2010.old		initial release
Function BankerRound( inVal, place, [toOdd] ) 
	variable inVal 			// floating point 
	variable place			// digits column to round to (0=1's place, 3= 10^3 =1000s, -3= 10^-3 =1/1000ths)
	variable toOdd 			// non-zero for round-to-odd
	
	variable maxdev = ( 0.00001 < 10^(place-5) ? 0.00001 : 10^(place-5) )
	// R ~= 1/2? chose 5+ places smaller than precision, at least within 1/100,000th of 1/2
	
	variable R = mod(inVal, 10^place)/10^place		 		// calc normalized remainder R (range 0 to +/-1)
	if ( abs(abs(R) - 0.5) <= maxdev ) 						// if very close to +/-0.5
		// make inVal (+) integer, modulo by 2 to determine inVal as even/odd (0=even, 1=odd)
		// evaluate toOdd boolean expression (0=round-to-even, 1=round-to-odd)
		// if inVal=even & round-to-even OR inVal=odd & round-to-odd, nudge R closer to zero
		// if inVal=odd & round-to-even OR inVal=even & round-to-odd, nudge R away from zero
		R += ( mod(trunc(abs(inVal)),2)==(toOdd!=0) ) ? -0.2*sign(R) : 0.2*sign(R)
	endif
	inVal /= 10^place 										// move decimal to target place
	inVal = ( abs(R) < 0.5 ) ? trunc(inVal) : round(inVal) 		// if |R|<1/2, round to 0, otherwise round away 0
	inVal *= 10^place										// move decimal back to starting place & return
	return inVal											// return rounded inVal 
End 


// Returns a string with the bits in <var> written MSB->LSB as 4bit blocks
// arbitrarily limited to 32 bits. change variable <upperBound> if desired
//
// 2013.05.28 	FIX error causing leading 1 to be truncated for certain values
// 2013.05.23 	major API behavior & changes
//	 				- arg `howMany` has been removed; function automatically determines length of
//						resulting string now
//					- new arg `minLen` can be used to get leading zeros if desired
// 					- new arg `wordsize` determines grouping within string (1110 0100 vs 11 10 01 00)
//						defaults to four
//					- arg `maxlen` operates generally as expected; also, it overrides minlen (!) if max<min
// 2011.09		initial release
Function/S BitString( num [, wordsize, minLen, maxLen] )
	variable num, wordsize, minLen, maxLen
	variable i
	string out = ""
	num = trunc(num)
	wordsize = ( ParamIsDefault(wordsize) ? 4 : wordsize )
	minLen = Max((ParamIsDefault(minLen) ? 0 : minLen), ceil(log(num+1)/log(2)))
	// shift of num ---> +1 is necessary so binary values with a single leading zero (2 = 10, 4=100, 8=1000, ...)
	// are not truncated by the minLen calculation:
	//		1000 = 8 = 2^3 
	//		1001 = 9 = 2^3.16
	//		1111 = 15 = 2^3.9	<-- round exponent up (ceiling) to get # of digits in bit string
	//		10000 = 16 = 2^4
	// for special case of leading one in binary, the exponent calculated this way is incorrect. if, instead, the 
	// _following_ integer is used, then ceiling rounding is dependable & legitimate. Observe:
	//		1000 = 8 ----> 9 = 2^3.16 ----> 4 
	//		1001 = 9 ----> 10 = 2^3.32 ----> 4 
	//		1111 = 15 ----> 16 = 2^4 ----> 4
	//		10000 = 16 ----> 17 = 2^4.08 ----> 5 
	maxLen = ( ParamIsDefault(maxLen) ? minLen : maxLen )
	for ( i=Limit(minLen, 1, maxLen)-1; i>=0; i-=1 )
		sprintf out, "%s%d", out, ( (num & (2^i)) != 0 )
		if ( !Mod( i, wordsize ) )
			out += " "
		endif
	endfor
	return out // not threadsafe
End


// Returns numeric interpretation of identifiable cardinal directions (NW, S, SSE) or NAN 
//
// 2011.09		initial release
Function Cardinal2D( inStr )
	string inStr // not threadsafe
	variable outVal
	inStr = UpperStr(UnPadString(inStr, 0x20))	// remove trailing spaces, convert uppercase
	strswitch(inStr)	
		case "N":
			return 0
		case "NNE":
			return 22.5
		case "NE":
			return 45
		case "ENE":
			return 67.5
		case "E":
			return 90
		case "ESE":
			return 112.5
		case "SE":
			return 135
		case "SSE":
			return 157.5
		case "S":
			return 180
		case "SSW":
			return 202.5
		case "SW":
			return 225
		case "WSW":
			return 247.5
		case "W": 
			return 270
		case "WNW":
			return 292.5
		case "NW":
			return 315
		case "NNW":
			return 337.5
		default:	
			return NAN 
	endswitch
end


// Returns <var> with bit # <bit> set to 0
//
// derived directly from the demo under Using Bitwise Operators
//
// 2011.09		initial release
Function ClearBit( var, bit )
	variable var, bit
	return ( trunc(var) & ~(2^bit) )
End


// TODO
//	consider re-implementing the X-scale check
// 	add a wtype filter to pass-thru to WaveList
//
// concatenates waves of the same name from datafolders in <dfrlist>, placing them in <destDFR>
//
// 2012.02.28 	added feedback to differentiate between intial and subsequent additions to concat list;
//				added alert for empty (skipped) folders
// 2011.11.02 	fixed bug where folders were not killed with <kill=1> and added an empty-check
// 2011.10.28 	changed output destination from DFREF to string path
// 2010.old		derived from old function
Function ConcatAcrossDFRs( DFRlist, destPath, overwrite, kill, [wfilter] )
	wave/DF DFRlist		// ordered list of data folders to concatenate from
	string destPath		// string path to output folder, passed internally to NewDataFolderX
						//	"" 
	variable overwrite		// nonzero to overwrite existing data in output folder
	variable kill			// 	0 	do not kill sources
						//	1	kill sources after concatenation (memory conserving)
						// 	2	kill sources after finishing all concatenations (for paranoid androids)
	string wfilter			// optional filter passed to wavelist(); defaults to "*"
	
	wfilter = SelectString(ParamIsDefault(wfilter), wfilter, "*")
	DFREF sav0 = GetDataFolderDFR()
	DFREF dest = NewDataFolderX( destPath )
	variable i, j, nwaves, nlists, windex, endpnt, initialAdditions
	string wfound, wname, thislist
	Make/T/FREE/N=0 concatlist
	
	for (i=0; i<numpnts(DFRlist); i+=1)
		SetDataFolder DFRlist[i]
		wfound = WaveList(wfilter,";","")
		nwaves = ItemsInList(wfound)
		if ( !nwaves )
			print "* Warning: folder <"+GetDataFolder(1, DFRlist[i])+"> contained no waves - skipping"
			continue		// skip folder if no waves inside
		else
			print "Adding folder <"+GetDataFolder(1, DFRlist[i])+"> to source list"
		endif
		if ( numpnts(concatlist) )
			initialAdditions = 1
		endif
		for (j=0; j<nwaves; j+=1)
			wname = StringFromList(j, wfound)
			windex = FindDimLabel(concatlist, 0, wname)
			If (windex < -1)
				endpnt = numpnts(concatlist)
				InsertPoints endpnt, 1, concatlist
				SetDimLabel 0, endpnt, $wname, concatlist
				concatlist[endpnt] = GetDataFolder(1)+wname+";"
				If ( initialAdditions )
					print "\t* Warning: added previously unfound wave to list: "+wname
				else
					print "\tAdded wave to concatenation list: "+wname
				endif
			else
				concatlist[windex] += GetDataFolder(1)+wname+";"
			endif
		endfor
	endfor
	SetDataFolder sav0
	
	for (i=0; i<numpnts(concatlist); i+=1)
		thislist = concatlist[i]
//		// EXCERPTED FROM OLD CODE - RELIABILITY SUSPECT
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
//		// END EXCERPT		
		wname = GetDimLabel(concatlist, 0, i)
		If (overwrite && kill == 1)
			Concatenate/NP/KILL/O thislist, dest:$wname
		elseif (overwrite && kill != 1)
			Concatenate/NP/O thislist, dest:$wname
		elseif (!overwrite && kill ==1)
			Concatenate/NP/KILL thislist, dest:$wname
		else
			Concatenate/NP thislist, dest:$wname
		endif
	endfor
	
	If (kill)
		for (i=0; i<numpnts(DFRlist); i+=1)
			DFREF dfr = DFRlist[i]
			If (CountObjectsDFR(dfr,1) || CountObjectsDFR(dfr,2) || CountObjectsDFR(dfr,3) || CountObjectsDFR(dfr,4) )
				print "ConcatAcrossDFRs: could not kill data folder <"+GetDataFolder(0,dfr)+"> because it still contains objects"
			else
				KillDataFolder/Z DFRlist[i]
			endif
		endfor
	endif
	return 0
End


// Returns total number of NANs in wave
//
// 2011.10.11 	added Limit()s to point boundaries
// 2011.09		initial release
Function CountNans( wname, [p1, p2] )
	wave wname			// wave to examine 
	variable p1, p2		// optional point boundaries, inclusive
	variable i, tot
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(wname)-1 : p2), p1, numpnts(wname)-1 )
	for (i=p1; i<=p2; i+=1)
		tot += (numtype(wname[i])==2)
	endfor
	return tot
End


// Returns covariance of wx and wy as Cov(wx,wy) = mean(wx*wy) - mean(wx)*mean(wy)
// or NAN if any points were NAN or waves not same length
//
// 2011.11.21 	changed numpnts() to DimSize(); added multidimensional check
// 2011.10.11		added limits to point boundaries
// 2011.10		added point boundaries
// 2011.09		initial release
Function Cov( wx, wy, [p1, p2])
	wave wx, wy		// waves with values
	variable p1, p2	// optional range specifiers, default: full wave
	
	If ( !SameNumPnts(wx, wy) )
		print "Cov: waves <"+NameOfWave(wx)+"; "+NameOfWave(wy)+"> are not the same length - aborting"
		return NAN
	elseif ( WaveDims(wx)>1 || WaveDims(wy)>1 )
		print "Cov: cannot operate on multidimensional waves - aborting"
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(wx,0)-1 : p2), p1, DimSize(wx,0)-1 )
	Duplicate/FREE/R=[p1,p2] wx, wxwy
	wxwy[0, p2-p1] = wx[ p1+p ] * wy[ p1+p ] 
	variable foo = (mean(wxwy)-mean(wx,pnt2x(wx,p1),pnt2x(wx,p2))*mean(wy,pnt2x(wy,p1),pnt2x(wy,p2)))
	return foo
End


// returns cardinal representation (N, SE, WSW) of <inVal> which is wrapped into 0-360
//
// 2011.09		initial release
Function/S D2Cardinal( inVal )
	variable inVal
	Make/FREE/T outStr = {"N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"}
	SetScale/P x, 0, 22.5, "", outStr
	return outStr( ModWD(inVal) ) // not threadsafe, string
End


// returns <inVal> in radians
//
// 2010.old		initial release
ThreadSafe Function D2R( inVal )
	variable inVal
	return inVal*(PI/180)
End


// returns daqfactory timestamp converted to igor date/time
//
// DAQfactory internally stores time as # of secs since 1970 (01/01?) 
//
// 2011.11.07 	written
ThreadSafe Function daqfactory2secs( timeval )
	variable timeval
	return timeval + date2secs(1970, 1, 1)
End


// returns day of the week (1=Sun; ... 7=Sat)
//
// Jan 1, 1904 was a Friday
//
// 2011.11.17 	written
Function DayOfWeek( tstamp )
	variable tstamp					// igor date/time value		sec
	return mod( 5+trunc(tstamp/86400), 7 )+1
end


// returns density of air in g/m^3 or mol/m^3 using ideal gas law
//
//	PV=nRT >> (n/V) = P/(RT)		(n/V) = air density		mol / m^3		OR 	g / m^3
//								P = barometric presssure	mbar
//								R = gas constant			(mb m^3)/(mol K)	OR	(mb m^3)/(g K)
//								T = temp*				Celcius
//							*use ambient temp for dry air density, virtual temp for moist air density
//
// 2011.11.15		standarized input units
// 2011.11.09 	written
Function DensityOfAir( T_, P_ [, inMoles] )
	variable T_ 			// ambient or virtual temp. 			Celcius
	variable P_			// barometric press. 				mbar = hPa
	variable inMoles		// nonzero to return molar density
	If ( inMoles )
		return P_ / (kRu * (T_+273.15))
	else
		return P_ / (kR_dryair * (T_+273.15)) 
	endif
end


// applies soft spike filter to entire wave following method of HaPe Schmid; input wave IS modified; optional
// point boundaries permit applying filter to subsection of wave => see IntervalDespikeHaPe() for application to time series
//
// 	Schmid, HaPe, C. Susan B. Grimmond, Ford Cropley, Brian Offerle, and Hong-Bing Su. "Measurements 
// 	of CO2 and energy fluxes over a mixed hardwood forest in the mid-western United States." Agricultural 
// 	and Forest Meteorology. 103 (2000): 357-374.
// 		"For each 15min period and variable, the means and variances are calculated. From these diagnostics,
// 		a threshold for spikes is determined as a multiple of the standard deviation (3.6 S.D. initially, increased 
//		by 0.3 after each pass). On each pass, a soft spike is registered if the fluctuation from the mean is larger 
//		than the threshold value, and if the duration of the spike is three or fewer records, corresponding to a 
//		persistence of 0.3s, for the 10Hz sampling rate. Longer-lasting departures from the period mean are taken 
// 		to indicate possible physical events. After each pass, if spikes are detected, the mean and variance are 
//		adjusted to exclude data marked as spikes and the process repeated, until either there are no more new 
//		spikes or the maximum or three iterations is completed (which is rarely the case)."
//
// 2012.06.12 	added optional parameter to specify max persistence of registered spikes
// 2011.11.21 	split logic to create an Interval_ function and independent function
// 2011.11.18		adapted from legacy function 
Function/WAVE DespikeHaPe( wname, tstamp [, multiplier, increment, passes, duration, p1, p2] )
	wave wname							// target wave ref
	wave/Z/D tstamp						// double-precision Igor date-time wave; used to evalute spike persistence 
										// 	time; if wave does not exist, X-scaling of <wname> is used instead 
	variable multiplier 						// optionally specify different base multipier (def: 3.6)
	variable increment 					// optionally specify different per-pass increment (def: 0.3)
	variable passes 						// optionally specify number of passes (def: 3)
	variable duration 						// optionally specify max persistence to be spike, seconds (def: 0.3)
	variable p1, p2						// optional inclusive point boundaries
	
	If ( !WaveType(wname) || WaveDims(wname)>1 )
		print "DespikeHaPe: source wave <"+NameOfWave(wname)+"> is non-numeric or multidimensional - aborting"
	endif
	
	If ( !WaveExists(tstamp) )
		Make/D/FREE/N=(DimSize(wname,0)) tstamp = leftx(wname) + deltax(wname)*p
	endif
	multiplier = ParamIsDefault(multiplier) ? 3.6 : multiplier
	increment = ParamIsDefault(increment) ? 0.3 : increment
	passes = ParamIsDefault(passes) ? 3 : passes
	duration = ParamIsDefault(duration) ? 0.3 : duration
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(wname,0)-1 : p2), p1, DimSize(wname,0)-1 )
	
	variable pn, avg, sdev, found, mult, threshold, pp, pp0
	Make/FREE/N=(passes,2) results = 0
	SetDimLabel 1, 0, spikes, results
	SetDimLabel 1, 1, points, results

	for (pn=0; pn<passes; pn+=1)			// for each pass through time series
		avg = mean(wname, pnt2x(wname,p1), pnt2x(wname,p2))
		sdev = sqrt( variance(wname, pnt2x(wname,p1), pnt2x(wname,p2)) )		
		found = 0
		mult = multiplier + pn*increment
		threshold = abs(mult*sdev)
		for (pp=p1; pp<p2+1; pp+=1)		// for each point in subinterval
			If ( numtype(wname[pp]) )					// for NANs, just skip
				continue
			elseif ( abs(wname[pp] - avg) > threshold )	// if spike criterea is met
				pp0 = found ? pp0 : pp						// capture pp=>pp0 only when new spike observed
				found = found ? found : 1						// register spike is observed if not already
			elseif ( found )							// criterea no longer met but spike was observed
				found = 0
				If ( (tstamp[pp-1] - tstamp[pp0]) <= duration )
				//	print/D "spike registered between",pp0,"and",pp-1	
					wname[pp0, pp-1] = NAN
					results[pn][%spikes] += 1
					results[pn][%points] += (pp-pp0)
				endif
			endif
		endfor			// each point
	endfor			// each pass
	return results
End	


// returns dew point based on vapor pressure
//
// 	Bolton, D., 1980: The Computation of Equivalent Potential Temperature. Monthly Weather
//	Review. Vol 108, 1046-1053. 
//		Eqn	11: 	T = ( 243.5*ln(es) - 440.8 ) / ( 19.48 - ln(es) )			es = sat. vapor pressure, mbar
//
// 2011.11.08 written
Function DewPoint( e_ )
	variable e_ 		// H2O vapor pressure 		mbar = hPa
	return (243.5 * ln(e_) - 440.8) / (19.48 - ln(e_))	// Celcius
End


// creates boolean waves denoting presence or absence of diagnostics flags
//
// 2011.11.16		renamed from CPC3776diagnostics
// 2011.10.31		initial release
Function/WAVE DiagnoseCPC3776( diagWord, option )
	wave diagWord		// diagnostic word from CPC 3776
	variable option		// set bit true to make additional waves (511 = all)
						//	-	-		total of boolean flags		UINT8		W_cpc3776_flags
						//	0	(1)		saturator temp			UINT8 		W_cpc3776_sat_temp
						//	1	(2)		condensor temp			UINT8 		W_cpc3776_cond_temp
						//	2	(4)		optics temp 				UINT8 		W_cpc3776_opt_temp
						//	3	(8)		inlet flow rate				UINT8 		W_cpc3776_in_flow
						//	4 	(16) 	aerosol flow rate			UINT8 		W_cpc3776_aero_flow
						//	5 	(32)		laser power 				UINT8 		W_cpc3776_laser_pwr
						//	6 	(64) 	liquid level				UINT8 		W_cpc3776_liq_level
						// 	7 	(128) 	concentration			UINT8 		W_cpc3776_conc_flag
						//	8 	(256) 	calibration reminder		UINT8 		W_cpc3776_cal_remind
						//	...			[all other bits reserved]
	variable i, val, n = numpnts(diagWord)
	Make/B/U/FREE/N=(n) W_cpc3776_flags, W_cpc3776_sat_temp, W_cpc3776_cond_temp, W_cpc3776_opt_temp
	Make/B/U/FREE/N=(n) W_cpc3776_in_flow, W_cpc3776_aero_flow, W_cpc3776_laser_pwr, W_cpc3776_liq_level
	Make/B/U/FREE/N=(n) W_cpc3776_conc_flag, W_cpc3776_cal_remind
	variable npnts = numpnts(diagWord)
	for (i=0; i<npnts; i+=1)
		val = diagWord[i]
		if ( numtype(val) )
			continue
		endif
		W_cpc3776_sat_temp[i] 	= TestBit(val, 0)		// saturator temp
		W_cpc3776_cond_temp[i] 	= TestBit(val, 1) 		// condensor temp
		W_cpc3776_opt_temp[i] 	= TestBit(val, 2) 		// optics temp
		W_cpc3776_in_flow[i]		= TestBit(val, 3) 		// inlet flow rate
		W_cpc3776_aero_flow[i]	= TestBit(val, 4) 		// aerosol flow rate
		W_cpc3776_laser_pwr[i] 	= TestBit(val, 5) 		// laser power
		W_cpc3776_liq_level[i] 	= TestBit(val, 6) 		// liquid level
		W_cpc3776_conc_flag[i] 	= TestBit(val, 7) 		// concentration flag
		W_cpc3776_cal_remind[i] 	= TestBit(val, 8) 		// calibration reminder
		W_cpc3776_flags[i] = W_cpc3776_sat_temp[i]+W_cpc3776_cond_temp[i]+W_cpc3776_opt_temp[i]+W_cpc3776_in_flow[i]
		W_cpc3776_flags[i] 	+= W_cpc3776_aero_flow[i]+W_cpc3776_laser_pwr[i]+W_cpc3776_liq_level[i]
		W_cpc3776_flags[i] 	+= W_cpc3776_conc_flag[i]+W_cpc3776_cal_remind[i]
	endfor
	Duplicate/B/U/O W_cpc3776_flags, :W_cpc3776_flags
	If ( TestBit(option, 0) )
		Duplicate/B/U/O W_cpc3776_sat_temp, :W_cpc3776_sat_temp
	endif
	If ( TestBit(option, 1) )
		Duplicate/B/U/O W_cpc3776_cond_temp, :W_cpc3776_cond_temp
	endif
	If ( TestBit(option, 2) )
		Duplicate/B/U/O W_cpc3776_opt_temp, :W_cpc3776_opt_temp
	endif
	If ( TestBit(option, 3) )
		Duplicate/B/U/O W_cpc3776_in_flow, :W_cpc3776_in_flow
	endif
	If ( TestBit(option, 4) )
		Duplicate/B/U/O W_cpc3776_aero_flow, :W_cpc3776_aero_flow
	endif
	If ( TestBit(option, 5) )
		Duplicate/B/U/O W_cpc3776_laser_pwr, :W_cpc3776_laser_pwr
	endif
	If ( TestBit(option, 6) )
		Duplicate/B/U/O W_cpc3776_liq_level, :W_cpc3776_liq_level
	endif
	If ( TestBit(option, 7) )
		Duplicate/B/U/O W_cpc3776_conc_flag, :W_cpc3776_conc_flag
	endif
	If ( TestBit(option, 8) )
		Duplicate/B/U/O W_cpc3776_cal_remind, :W_cpc3776_cal_remind
	endif
	wave outref = W_cpc3776_flags
	return outref
End


// creates boolean waves denoting presence or absence of diagnostics flags
//
// 2013.05.28 	*API change* no longer creates wave by default; user has responsibliity to save results
// 2013.05.23 	*API change* made arg `option` optional
// 2011.11.16 	renamed from CSAT3diagnostics()
// 2011.10.31		reduced If(...) structure to boolean additions under default switch statment
// 2011.09		initial release
Function/WAVE DiagnoseCSAT3( diagWord [, option] )
	wave diagWord		// diagnostic word from CSAT3
	variable option		// set bit true to make additional waves (15=all)
						//	0	(1)		speed of sound diff. boolean	W_csat_del_T
						//	1	(2)		poor signal lock boolean		W_csat_sig_lck
						//	2	(4)		high amplitude boolean		W_csat_amp_h
						//	3	(8)		low amplitude boolean			W_csat_amp_l
						//	...			[all other bits reserved]
	variable i, val, n = numpnts(diagWord)
	option = (ParamIsDefault(option) ? 0 : option)
	Make/B/U/FREE/N=(n) W_csat3_flags, W_csat3_del_T, W_csat3_sig_lck, W_csat3_amp_h, W_csat3_amp_l
	for (i=0; i<numpnts(diagWord); i+=1)
		val = diagWord[i]
		if ( numtype(val) )
			continue
		endif
		switch (val)
			case -99999:
			case 61440:
			case 61503:
			case 61441:
			case 61442:
				W_csat3_flags[i] += 1
				break;
			default:
				W_csat3_del_T[i] 	= TestBit(val, 15) 		// speed of sound diff
				W_csat3_sig_lck[i] 	= TestBit(val, 14)		// poor signal lock
				W_csat3_amp_h[i] 	= TestBit(val, 13)		// high amp
				W_csat3_amp_l[i] 	= TestBit(val, 12) 		// low amp
				W_csat3_flags[i] = W_csat3_del_T[i]+W_csat3_sig_lck[i]+W_csat3_amp_h[i]+W_csat3_amp_l[i]
		endswitch
	endfor
	//Duplicate/B/U/O W_csat3_flags, :W_csat3_flags
	If ( TestBit(option, 0) )
		Duplicate/B/U/O W_csat3_del_T, :W_csat3_del_T
	endif
	If ( TestBit(option, 1) )
		Duplicate/B/U/O W_csat3_sig_lck, :W_csat3_sig_lck
	endif
	If ( TestBit(option, 2) )
		Duplicate/B/U/O W_csat3_amp_h, :W_csat3_amp_h
	endif
	If ( TestBit(option, 3) )
		Duplicate/B/U/O W_csat3_amp_l, :W_csat3_amp_l
	endif
	wave outref = W_csat3_flags
	return outref
End


// creates boolean waves denoting presence or absence of diagnostics flags
//
// 2013.05.28 	**API CHANGES** 
//				- made arg `option` optional
// 				- no longer creates wave by default -- user's responsibility to save the results now
// 					using Duplicate or MoveWave
// 2011.11.16 	renamed from LI7500diagnostics()
// 2011.10.31		initial release
Function/WAVE DiagnoseLI7500( diagWord [, option] )
	wave diagWord		// diagnostic word from LI-7500
	variable option		// set bit true to make additional waves (31=all)
						//	-	-		total of boolean flags		UINT8		W_li7500_flags
						//	0	(1)		chopper warning			UINT8 		W_li7500_chopper
						//	1	(2)		detector warning			UINT8 		W_li7500_detector
						//	2	(4)		phase lock loop warning	UINT8 		W_li7500_pll
						//	3	(8)		sync warning				UINT8 		W_li7500_sync
						//	4 	(16) 	AGC value				SP 			W_li7500_AGC
						//	...			[all other bits reserved]
	variable i, val, n = numpnts(diagWord)
	option = (ParamIsDefault(option) ? 0 : option)
	Make/B/U/FREE/N=(n) W_li7500_flags, W_li7500_chopper, W_li7500_detector, W_li7500_pll, W_li7500_sync
	Make/FREE/N=(n) W_li7500_AGC
	for (i=0; i<numpnts(diagWord); i+=1)
		val = (diagWord[i] %^ 0xF0 )		// swap bits 4-7: 1111 0000 so "OK" is now 0 and "flag" is now 1
		if ( numtype(val) )
			continue
		endif
		W_li7500_chopper[i] 	= TestBit(val, 7) 		// chopper
		W_li7500_detector[i] 	= TestBit(val, 6)		// detector 
		W_li7500_pll[i] 		= TestBit(val, 5) 		// phase lock loop not OK
		W_li7500_sync[i] 	= TestBit(val, 4)		// sync not OK
		W_li7500_flags[i] = W_li7500_chopper[i]+W_li7500_detector[i]+W_li7500_pll[i]+W_li7500_sync[i]
		W_li7500_AGC[i] 	= ( val & 0x0F ) * 6.25 
	endfor
	//Duplicate/B/U/O W_li7500_flags, :W_li7500_flags
	If ( TestBit(option, 0) )
		Duplicate/B/U/O W_li7500_chopper, :W_li7500_chopper
	endif
	If ( TestBit(option, 1) )
		Duplicate/B/U/O W_li7500_detector, :W_li7500_detector
	endif
	If ( TestBit(option, 2) )
		Duplicate/B/U/O W_li7500_pll, :W_li7500_pll
	endif
	If ( TestBit(option, 3) )
		Duplicate/B/U/O W_li7500_sync, :W_li7500_sync
	endif
	If ( TestBit(option, 4) )
		Duplicate/O W_li7500_AGC, :W_li7500_AGC
	endif
	wave outref = W_li7500_flags
	return outref
End


// averages values by day; does not handle NANs yet
//
// 2011.11.17 	added <mode> to omit days if desired
// 2011.11.09 	written
Function/WAVE DielAverage( wname, tstamp, mode)
	wave wname			// value wave
	wave/D tstamp		// time wave
	variable mode		// bitmask: set bit HI to exclude that day of the week
						//		weekends only = 2+4+8+16+32 = 62
						//		weekdays only = 1+64 = 65
						//	bit	value	DOW omitted		bit	value	DOW omitted
						//	-	0		-none-				3	8		Wednesday
						//	0	1		Sunday 				4	16 		Thursday
						//	1	2		Monday 				5	32		Friday
						//	2	4		Tuesday 			6	64 		Saturday
	variable i, lo, hi, oi
	If ( mode )
		for (i=0; i<7; i+=1)
			If ( TestBit( mode, i ) )
				Extract/FREE wname, tmp0, DayOfWeek(tstamp) != (i+1)
				Extract/D/FREE tstamp, tmp1, DayOfWeek(tstamp) != (i+1)
				wave wname = tmp0
				wave/D tstamp = tmp1
			endif
		endfor
	endif
	Duplicate/D/FREE tstamp, tdiel
	tdiel = mod(tstamp, 86400)
	Duplicate/FREE wname, sortw
	Sort tdiel, tdiel, sortw
	
	Make/FREE/N=( 86400 / (tstamp[1]-tstamp[0]) ) dielw = NAN
	SetScale/P x, 0, (tstamp[1]-tstamp[0]), "dat", dielw	
	for (i=0; 1; i+=1)
		if ( i+1 >= DimSize(tdiel,0) )
			dielw[oi] = mean(sortw, pnt2x(sortw,lo), pnt2x(sortw, i ) )
			break
		elseif ( tdiel[i+1] > tdiel[i] )
			hi = i
			dielw[oi] = mean(sortw, pnt2x(sortw,lo), pnt2x(sortw,hi) )
			lo = i+1
			oi += 1
		endif
	endfor
	return dielw
End


// converts decimal day of year into igor date/time
Function doy2sec( doy, year )
	variable doy		// day of year, indexed from 1
	variable year		// year is reqd since doy depends on year

	year = trunc(year)
	Make/FREE/N=366 monthmap = NAN; 	// monthmap[0] not used
	monthmap[1,31]=1;		monthmap[32,59]=2;		monthmap[60,90]=3
	monthmap[91,120]=4;		monthmap[121,151]=5;	monthmap[152,181]=6
	monthmap[182,212]=7; 	monthmap[213,243]=8;	monthmap[244,273]=9
	monthmap[274,304]=10;	monthmap[305,334]=11; 	monthmap[335,365]=12
	Make/FREE/N=13 endDOY = {NAN, 31, 59, 90, 120, 151, 181, 212, 242, 273, 304, 334, 365}
	
	If ( !mod(year,4) && ( mod(year,100) || !mod(year,400) ) )	// LEAP YEAR ADJ
	// if (year % 4 == 0) AND ((year % 100 !=0) OR (year % 400 == 0))
		InsertPoints 60, 1, monthmap; 	
		monthmap[60] = 2
		endDOY[2, 12] += 1
		doy = mod(doy, 366)
	else
		doy = mod(doy, 365)
	endif
	variable mo = monthmap[trunc(doy)]
	return date2secs(year, mo, (doy - endDOY[mo-1])) + mod(doy,1)*86400
End


// TODO
// 	verify this function
//
// returns latent heat flux using eddy covariance
//
// 2011.11.11 	written
Function ECLatentHeat( h2o, w_ [, T_, p1, p2] )
	wave h2o 			// water vapor molar density 		g / m^3
	wave w_ 			// vertical wind					m/s
	wave T_ 				// optional ambient temp 			Celcius
	// temp determines latent heat of vaporization as f(meanT); uses 2.5e3 J/g otherwise
	variable p1, p2		// optional point boundaries, inclusive
	
	If ( !SameNumPnts(h2o, w_) || (WaveExists(T_) && !SameNumPnts(h2o, T_)) )
		print "ECLatentHeat: input waves were not the same length - aborted"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(h2o,0) : p2), p1, DimSize(h2o,0) )
	variable out = Cov( h2o, w_, p1=p1, p2=p2 )
	If ( ParamIsDefault(T_) || !WaveExists(T_) )
		out *= 2.5e3
	else
		out *= LatentHeatVapH2O(mean(T_,pnt2x(T_,p1),pnt2x(T_,p2)))
	endif
	return out
End


// returns sensible heat flux in Watts/meter^2 using eddy covariance
//
// weak source: Eqn 11.20 Arya, S. Pal. Introduction to Micrometerology. 2nd Ed. 2001. Academic Press.
//
// 2013.05.20 	fix numpnts check
// 2012.06.18 	*bug* fixed units scaling which previously resulted in values 100X too small
// 2011.11.11 	written
Function ECSensibleHeat( T_, w_ , P_, Q_, [p1, p2] )
	wave T_	 			// ambient temp.					Celcius
	wave w_ 			// vertical wind					m/s
	wave P_ 			// ambient press.					mbar
	wave Q_ 			// specific humidity 				dimensionless, 0-1
	variable p1, p2		// optional point boundaries, inclusive
	
	Make/FREE/WAVE/N=5 wlist = {T_, w_, P_, Q_}
	If ( !SameNumPntsW( wlist ) )
		print "ECSensibleHeat: input waves were not the same length - aborted"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(T_,0) : p2), p1, DimSize(T_,0) )
	variable out, mt, mq, mp
	mt = mean(T_, pnt2x(T_,p1), pnt2x(T_,p2) )
	mq = mean(Q_, pnt2x(Q_,p1),pnt2x(Q_,p2) )
	mp = mean(P_, pnt2x(P_,p1),pnt2x(P_,p2) )
	out = 100 * kCp_dryair * (1+0.84*mq) * DensityOfAir( VirtualTemp( mt, mq ), mp ) * Cov( T_, w_, p1=p1, p2=p2 ) 
	return out
End


// return eddy covariance flux of CO2 from gas density and wind measurements
// value will be NAN if arguments contain NAN
//
// 2013.05.20 	written
Function EC_co2( co2, w_, [p1, p2] )
	wave co2 			// density of carbon dioxide			g / m^2 or mol/m^2
	wave w_				// vertical wind speed 					m / s
	variable p1, p2 		// optional point boundaries, inclusive
	
	If ( !SameNumPnts(co2, w_) )
		print "EC_co2: input waves are not all the same length - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(co2,0)-1 : p2), p1, DimSize(co2,0)-1 ) 
	
	return Cov( co2, w_, p1=p1, p2=p2 )
End



// TODO
//	verify results of this function
//
// returns eddy covariance density corrections following WPL(80) using variables 
// value returned is complex: water vapor term resides in real portion, temp. term in imaginary portion
//
// 	<Fc> = <Fco> + <RHOc> * [ mu*Cov( W, RHOv ) / <RHOd> + (1+ mu*sigma)*Cov( W, T ) / <T> ]
//					|_mult_|		|___water vapor term______|	 |___temperature term_________|
//
//			Fco 	uncorrected flux 							g m-2 s-1
//			RHOc 	density of constituent C					g m-3
//			mu		(MW dry air / MW water vapor) = 1/0.622	dimensionless
//			W 		vertical wind speed						m s-1
//			RHOv 	density of water vapor						g m-3
//			RHOd	density of dry air							g m-3
//			sigma 	= RHOv / RHOd							dimensionless
//			T 		temperature								Kelvin
//			<...> denotes averaging operation
//
// 2011.11.22 	written	
Function/C EC_WPL80( meanRHOc, meanRHOv, meanRHOd, meanT, cov_w_rhoV, cov_w_T )
	variable meanRHOc	// mean mass density of constituent C 		g / m^3
	variable meanRHOv	// mean mass density of water vapor 		g / m^3
	variable meanRHOd	// mean mass density of dry air				g / m^3
	variable meanT 		// mean temperature						Celcius
	variable cov_w_rhoV 	// covariance of vertical wind and rhoV 		g m-2 s-1
	variable cov_w_T 		// covariance of vertical wind and temp		C m / s = K m / s

	variable mu = kMW_dryair / kMW_h2o
	variable W_corr = mu * cov_w_rhoV / meanRHOd
	variable T_corr = (1 + mu*(meanRHOv / meanRHOd)) * cov_w_T / meanT
	
	return cmplx( meanRHOc*W_corr, meanRHOc*T_corr )
End


// TODO
//	verify results of this function
//
// returns eddy covariance density corrections following WPL (1980) using time series
// value returned is complex: water vapor term resides in real portion, temp. term in imaginary portion
//
// 	<Fc> = <Fco> + <RHOc> * [ mu*Cov( W, RHOv ) / <RHOd> + (1+ mu*sigma)*Cov( W, T ) / <T> ]
//					|_mult_|		|___water vapor term______|	 |___temperature term_________|
//
//			Fco 	uncorrected flux 							g m-2 s-1
//			RHOc 	density of constituent C					g m-3
//			mu		(MW dry air / MW water vapor) = 1/0.622	dimensionless
//			W 		vertical wind speed						m s-1
//			RHOv 	density of water vapor						g m-3
//			RHOd	density of dry air							g m-3
//			sigma 	= RHOv / RHOd							dimensionless
//			T 		temperature								Kelvin
//			<...> denotes averaging operation
//
// 2011.11.22 	written
Function/C EC_WPL80_TS( rhoC, rhoV, rhoD, T_, w_ [, p1, p2] )
	wave rhoC 			// density of constituent C 				g / m^3
	wave rhoV 			// density of water vapor 				g / m^3
	wave rhoD 			// density of dry air					g / m^3 
	wave T_ 				// ambient temperature				Celcius
	wave w_ 			// vertical wind component 				m / s
	variable p1, p2 		// optional inclusive point boundaries
	
	Make/FREE/WAVE/N=5 wlist = {rhoC, rhoV, rhoD, T_, w_}
	If ( !SameNumPntsW( wlist ) )
		print "EC_WPL80_TS: input waves were not all the same length - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(rhoC,0)-1 : p2), p1, DimSize(rhoC,0)-1 ) 

	variable meanC, meanV, meanD, cov_w_V, cov_w_T, meanT
	meanC = mean(rhoC, pnt2x(rhoC,p1), pnt2x(rhoC,p2))
	meanV = mean(rhoV, pnt2x(rhoV,p1), pnt2x(rhoV,p2))
	meanD = mean(rhoD, pnt2x(rhoD,p1), pnt2x(rhoD,p2))
	meanT = mean(T_, pnt2x(T_,p1), pnt2x(T_,p2)) + 273.15		// Celcius => Kelvin
	cov_w_V = Cov( w_, rhoV, p1=p1, p2=p2 )
	cov_w_T = Cov( w_, T_, p1=p1, p2=p2 )						// covariance unaffected by Celcius vs. Kelvin
	return EC_WPL80( meanC, meanV, meanD, meanT, cov_w_V, cov_w_T )
End


// returns estimate of record lag between two waves or NAN for error
//
// 2011.09-10 	initial implementation
Function EstimateLag( baseWave, targetWave, keepResults )
	wave baseWave		// reference timeseries
	wave targetWave		// time series to estimate time difference of (in record #s)
	variable keepResults	// nonzero to have correlation wave dumped in CDF, negative to display plot
	
	string nameB = NameOfWave(baseWave)
	string nameT = NameOfWave(targetWave)
	string destName = "corr_"+nameB+"_"+nameT
	
	If ( WaveRefsEqual(baseWave, targetWave) )
		print "EstimateLag: no lag to esimate between duplicate references <"+nameB+"; "+nameT+">"
		return NAN
	elseif ( !SameNumPnts(baseWave, targetWave) )
		print "EstimateLag: expected waves <"+nameB+"; "+nameT+"> to have same number of points - aborting."
		return NAN
	endif

	DFREF sav0 = GetDataFolderDFR()
	DFREF work = NewFreeDataFolder
	variable X0, foreX, foreVal, backX, backVal, xshift
	
	Duplicate/FREE baseWave, baseW
	Duplicate/FREE targetWave, targetW
	SetScale/P x, 0, 1, "", baseW, targetW
	
	Make/FREE/WAVE/N=2 nanlist = {baseW, targetW}
	RemoveNansW( nanlist )
	
	// Linear correlation:		[mean removal]		[auto corr. scaling]
	//		[with]				yes					yes or no		=> works ok, look near X=0
	//		[without]				no					yes or no 		=> garbage, triangle plot
	// Circular correlation		[mean removal]
	//		[with]				yes or no							=> shows sharper peak after rotating 
	//																	results by +(numpnts/2) 
	Correlate/NODC/C targetW, baseW
	Rotate (numpnts(baseW)/2), baseW	// changes x-scale
	FindPeak/Q/R=(0,rightx(baseW)) baseW
	foreX = ( V_flag ) ? NAN : V_PeakLoc
	foreVal = ( V_flag ) ? NAN : V_PeakVal
	FindPeak/Q/R=(0,leftx(baseW)) baseW
	backX = ( V_flag ) ? NAN : V_PeakLoc
	backVal = ( V_flag ) ? NAN : V_PeakVal
	
	if ( numtype(backX) && numtype(foreX) )			// no peaks found on either side
		xshift = NAN
	elseif ( numtype(foreX) )						// peaks only found on backward shift side
		xshift = backX
		foreX = 0
	elseif ( numtype(backX) )						// peaks only found on forward shift side
		xshift = foreX
		backX = 0
	else
		if ( abs(backX) < (foreX) )
			if ( backVal < foreVal )
				print "EstimateLag: correlation value for X-shift",backX,"is nearest but is not highest value possible"
			endif
			xshift = backX
		elseif ( abs(backX) > foreX )
			if ( foreVal < backVal )
				print "EstimateLag: correlation value for X-shift",foreX,"is nearest but is not highest value possible"
			endif
			xshift = foreX
		endif
	endif
	
	If (keepResults)
		Duplicate/O baseW, $destName
	endif
	If (keepResults < 0)
		Display $destName
		SetAxis/A=2 left
		SetAxis bottom ( xshift < 0 ? 2*xshift : -5), ( xshift > 0 ? 2*xshift : 5)
		ModifyGraph grid(bottom)=1,nticks(bottom)=10,mode=4,mirror(bottom)=2,mirror(left)=2,nolabel(left)=1
		Label left "Un-normalized cross-covariance"
		Label bottom "Shift (# points) of "+nameT+" relative to "+nameB
		TextBox/C/N=text0/A=LT "\\Z09Circular correlation\rmean removed"
	endif
	return xshift
End


// converts "serial date" used by default in excel to igor date/time
//
// Excel stores dates internally in the form ddddd.ttttt where 	ddddd 	= # days since 1900/01/00 
//														ttttt 		= time as a fractional day
// If the number is entirely fractional, no date is associated. Note Excel has a bug whereby 1900 is a leap year.
//
// Excel can also be put in 1904-mode where ddddd = # of days since 1904/01/01 (generally for Mac compatibility)
// Conveniently, Igor was (probably) developed on a Mac and uses # of secs since 1904/01/01 so then it's just scaling.
//
// 2011.11.07 	changed name from "serial2secs"; improved description
// 2011.09		initial release
ThreadSafe Function excel2secs( serialdate, [use1904mode] )
	variable serialdate, use1904mode
	If ( use1904mode )
		return serialdate*86400
	else
		return (serialdate - 1462)*86400 	// 1904/01/01 - 1900/01/01 = 1461
	endif
End


// flattens file directories by recursively lifting contents out of subfolders
//
// 2011.10.13 	posted to IgorExchange.com snippets
// 2011.10 		written
Function FlattenDir( pathName, recurse, overwrite, [fileFilter, kill] )
	string pathName		// string name of existing path; use NewPath or Misc->New Path...
	variable recurse		// how many levels to flatten? use -1 for ALL, 0 does nothing
	variable overwrite 		// nonzero to overwrite files with existing names 
	string fileFilter		// optional: provide filter to only lift certain file types
						//	see <fileTypeOrExtStr> parameter from IndexedFile for OK formats
						//	defaults to "????"=ALL FILES if format is not acceptable (no warnings!!)
	variable kill			// optional: specify how subfolders are handled after flattening
						//	negative #s: leave subfolder structure alone (flattens files only)
						//	0: emptied subfolders deleted only (default)
						//	1: all subfolders deleted (and remaining contents!!)
						//	2: do not delete any subfolders
	PathInfo $pathName
	If ( !V_flag )
		print "FlattenDir: path <"+pathName+"> could not be found - aborting."
		return -1
	endif
	GetFileFolderInfo/D/Q/Z S_path
	If ( V_flag )
		print "FlattenDir: directory specified by path <"+pathName+"> could not be found - aborting."
		return -1
	endif
	
	variable i, j, status
	string here = RemoveEnding(S_path,":"), folderList, filesIn, foldersIn, thisFile, thisFolder, tmpPN, subdir, delcmd
	If ( ParamIsDefault(fileFilter) || !GrepString(fileFilter, "^[.]{1}[a-zA-Z0-9]*$|^[a-zA-Z0-9]{4}$|^[?]{4}$") )
		fileFilter = "????"		// ensure fileFilter is an acceptable value
	endif

	if ( recurse )	// do nothing if no more recursive levels
		folderList = IndexedDir( $pathName, -1, 1 )
		for (i=0; i<ItemsInList(folderList); i+=1)		// for each subfolder found in here...
			subdir = StringFromList(i, folderList)
			tmpPN = UniqueName("TmpPath", 12, i)
			NewPath/Q/O $tmpPN, subdir
			// [positive <recurse> eventually stops at 0; negative <recurse> stops at no more subfolders]
			status = FlattenDir(tmpPN, (recurse-1), overwrite, fileFilter=fileFilter, kill=kill)		// ...attempt to flatten it...
			if ( status )
				return status	
			endif
			
			filesIn = IndexedFile( $tmpPN, -1, fileFilter)	// ...pull out all files matching filter...
			for (j=0; j<ItemsInList(filesIn); j+=1)
				thisFile = StringFromList(j, filesIn)
				try
					if (overwrite)
						MoveFile/D/O/Z (subdir+":"+thisFile) as here
						AbortOnValue V_flag, 1
					else
						MoveFile/D/Z (subdir+":"+thisFile) as here
						If ( V_flag ) 																//
							print "SKIP FILE",thisFile,"BECAUSE NO OVERWRITE"	// DEBUG
						endif																	//
					endif
				catch
					print "FlattenDir: could not move file <"+subdir+":"+thisFile+"> for unknown reason"
				endtry
				print "MOVE FILE",thisFile,"FROM",subdir,"TO",here				// DEBUG
			endfor
			If ( kill < 0 )
				//KillPath $tmpPN
				continue		// ...and skip the rest if ignoring subfolder structure. Otherwise
			endif

			foldersIn = IndexedDir( $tmpPN, -1, 1 )	// ...pull any subsubfolders out of this subfolder...
			for (j=0; j<ItemsInList(foldersIn); j+=1)
				thisFolder = StringFromList(j, foldersIn)
				try
					MoveFolder/D/Z thisFolder as here
					AbortOnValue V_flag, 1
					print "MOVE DIR",thisFolder,"FROM",subdir,"TO",here			// DEBUG
				catch
					print "FlattenDir: could not move folder <"+thisFolder+"> for unknown reason"
				endtry
			endfor
			switch (kill)		// ... then...
				case 2:	 		// ...do not delete this subfolder
					break
				default: 
				case 0:			// ...do not delete this subfolder if it has files in it
					If ( ItemsInList(IndexedFile($tmpPN, -1, "????")) )	
						print "SKIP DIR",subdir,"BECAUSE NOT EMPTY"						// DEBUG
						break
					endif
				case 1:			// ...delete this subfolder
					strswitch ( IgorInfo(2) )
						case "Windows":
							try
								delcmd = "recycle.exe -f "+ParseFilePath(5, subdir, "\\", 0, 0)
								ExecuteScriptText/Z delcmd
								AbortOnValue V_flag, 1
								print "RECYCLE DIR",subdir									// DEBUG
							catch
								print "FlattenDir: could not recycle folder <"+subdir+"> for unknown reason"
							endtry
							break
						case "Macintosh":
							try	// >> 	this block adapted from work by aclight via IgorExchange.com
								string posixPath = ParseFilePath(5, subdir, "/", 0, 0)
								if (strlen(posixPath) > 0)
									sprintf delcmd, "tell application \"Finder\"  to move ((POSIX file \"%s\") as alias) to trash", posixPath
									ExecuteScriptText/Z delcmd
									AbortOnValue V_flag, 1				
								endif
							catch
								print "FlattenDir: could not move folder <"+subdir+"> to trash for unknown reason"			
							endtry	// <<
							break
					endswitch
			endswitch
			KillPath $tmpPN
		endfor
	endif
	return 0
End


// returns friction velocity
//	http://amsglossary.allenpress.com/glossary/search?p=1&query=friction+velocity
//
// 	u* = [ Cov( u, w )^2 + Cov( v, w )^2 ]^(1/4)
//
// 2011.11.09 	written
Function FrictionVelocity( u_, v_, w_, [p1, p2] )
	wave u_, v_			// time series of horizontal wind components		m/s
	wave w_ 			// time series of vertical wind component			m/s
	variable p1, p2		// optional point boundaries, inclusive
	
	If ( !SameNumPnts(u_, v_) || !SameNumPnts(v_, w_) )
		print "FrictionVelocity: waves were not all same length - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(u_,0) : p2), p1, DimSize(u_,0) )	
	variable foo = sqrt( sqrt( Cov(u_, w_, p1=p1, p2=p2)^2 + Cov(v_, w_, p1=p1, p2=p2)^2 ) )
	return foo
End


// returns sorted, hierarchal list of all datafolders
// 
// 2011.10.13		renamed GetFolderList to GetDataFolderList
// 2010.old 		initial release
Function/S GetDataFolderList( [seePkgs] )
	variable seePkgs 		// nonzero to include root:Packages: and subfolders
	
	DFREF savDF = GetDataFolderDFR()
	SetDataFolder root:
	string str = "root:;" + ListDataFoldersIn("root:")	
	str = SortList(str, ";", 8)
	SetDataFolder savDF	
	If ( seePkgs )
		return str 
	else
		return GrepList(str, "Packages", 1)
	endif
End


// returns element number of first found duplicate DF reference in <wrefs>
//
// 2011.old		initial release
Function HasDuplicateDFRefs( dfrefs )
	wave/DF dfrefs
	variable i,j
	for (i=0; i<numpnts(dfrefs); i+=1)	
		for(j=i+1; j<numpnts(dfrefs); j+=1)
			if (DataFolderRefsEqual(dfrefs[i], dfrefs[j]))
				return j
			endif
		endfor
	endfor
	return 0
End


// returns element number of first found duplicate wave reference in <wrefs>
//
// 2011.old		initial release
Function HasDuplicateWRefs( wrefs )
	wave/WAVE wrefs
	variable i,j
	for (i=0; i<numpnts(wrefs)-1; i+=1)	
		for(j=i+1; j<numpnts(wrefs); j+=1)
			if (WaveRefsEqual(wrefs[i], wrefs[j]))
				return j	// not i because it could still be 0
			endif
		endfor
	endfor
	return 0
End


// returns element number of first NAN found or 0 if none
// special case element[0] returns -1
//
// 2011.11.22 	removed /D flag from <wname> param
// 2011.10.11		revised Limit()s for point boundaries
// 2011.10.07		added optional point range parameters
// 2011.old		initial release
Function HasNans( wname, [p1, p2] )
	wave wname		// wave to search in
	variable p1, p2		// optional point boundaries (default to full range)
	variable i
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(wname)-1 : p2), p1, numpnts(wname)-1 )
	for (i=p1; i<=p2; i+=1)
		if (numtype(wname[i])==2)
			return ( i ? i : -1 ) // ensures 0 isn't returned if @ 1st element
		endif
	endfor
	return 0
End


// returns element number of first NAN found or 0 if none
// special case element[0] returns -1
//
// 2012.06.13 	fixed bug where numtype comparison mistakenly used <i> as the index, not <j>
// 2011.11.22 	derived from HasNans
Function HaveNans( wrefs, [p1, p2] )
	wave/WAVE wrefs 			// references of waves to affect
	variable p1, p2				// optional point boundaries (default to full range)

	If ( !SameNumRowsW( wrefs ) )
		print "HaveNans: source waves had different numbers of rows - aborting"
		return NAN
	elseif ( !numpnts(wrefs) )
		print "HaveNans: source wave array had no waves referenced - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(wrefs[0])-1 : p2), p1, numpnts(wrefs[0])-1 )
	variable i,j
	for (i=0; i<DimSize(wrefs,0); i+=1)
		wave this = wrefs[i]
		for (j=p1; j<=p2; j+=1)
			if ( numtype(this[ j ]) ==2 )
				return ( j ? j : -1 ) // ensures 0 isn't returned if @ 1st element
			endif
		endfor
	endfor
	return 0
End


// TODO:
//	add optional edge parameter to permit end-of-period as well
//
// returns 2D wave describing time intervals found in <tstamp>. One row for each subinterval found; first column 
// is lower point, second column is upper point, both inclusive. The subintervals are of size <interval> in seconds,
// and optionally <aligned> to multiples of the interval. NANs or non-consecutive values in <tstamp> returns NAN
// 
// Note: When looping through the results wave, use DimSize() instead of numpnts()s to get at the number of rows
// instead of total number of points.
//
// 2011.10.11		added initial NAN check before numInts = ...
// 2011.09		initial release
Function/WAVE IntervalBoundaries(tstamp, interval, aligned) // not threadsafe, wave
	wave/D tstamp			// double precision wave
	variable interval			// interval size in seconds
	variable aligned			// nonzero to align intervals on midnight
	variable Lp, Ltime, Htime, numInts, oi, i, leftxval
	
	If ( numtype(tstamp[0]) || numtype(tstamp[Inf] ) )
		print/D "IntervalBoundaries: wave <"+NameOfWave(tstamp)+"> contained NAN in first or last point - aborting."
		return NAN
	endif
	numInts = CEIL( (tstamp[Inf] - tstamp[0]) / interval )+1
	Make/FREE/N=(numInts, 2) results
	SetDimLabel 1, 0, lo, results
	SetDimLabel 1, 1, hi, results

	Ltime = tstamp[Lp]
	if (aligned)
		for (Htime = Ltime - mod(Ltime,86400); Htime <= Ltime; Htime += interval)
		endfor // set high time bound to previous midnight, then advance by interval size until exceeds low time bound
		leftxval = Htime - interval
	else
		Htime = Ltime+interval
		leftxval = Ltime
	endif
	for (i=0; i<numpnts(tstamp); i+=1)			// check one point-at-a-time
		If ( numtype(tstamp[i]) )					// if NAN or INF
			print/D "IntervalBoundaries: wave <"+NameOfWave(tstamp)+"> has nonnumeric value at row",i,"- aborting."
			return NAN
		elseif ( i && (tstamp[i-1] >= tstamp[i]) )		// if last value wasn't lower
			print/D "IntervalBoundaries: wave <"+NameOfWave(tstamp)+"> is not consecutive at row",i,"- aborting."
			return NAN
		elseif ( tstamp[i+1] >= Htime || i+1 == numpnts(tstamp) ) // if next row is not in interval or @ end of wave
			results[oi][%lo] = Lp						// store lo bound
			results[oi][%hi] = i						// store hi bound
			oi += 1									// advance row index
			Lp = i+1									// set new lo to next point
			for ( Ltime=Htime; Ltime+interval<=tstamp[Lp]; Ltime+=interval) // so long as an interval will be skipped
				results[oi][%lo] = NAN					// skip it in output also
				results[oi][%hi] = NAN
				oi += 1
			endfor
			If (aligned)
				for (Htime = Ltime - mod(Ltime,86400); Htime <= Ltime; Htime += interval)
				endfor // set high time bound to previous midnight, then advance by interval size until exceeds low time bound
			else
				Htime = Ltime+interval
			endif
		endif
	endfor 
	if (numInts >= oi)
		DeletePoints oi, (numInts - oi + 1), results
	endif
	SetScale/P x, leftxval, interval, "dat", results
	return results
End


// calculates covariance of <wx> and <wy> over subintervals of size <interval>
// and optionally <aligned>. 
//
// returns wave with results / time set in X scale or NAN if <wx>, <wy> are different length
//
// 2011.11.22 	changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st=>3rd parameter
// 2011.10.10 	made insensitive to NANs
// 2011.09		initial release
Function/WAVE IntervalCov( wx, wy, tstamp, interval, aligned, [bp] )
	wave wx, wy			// X & Y waves to covary
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of subinterval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional 2D wave with start/stop points as returned by IntervalBoundaries
	
	If ( !SameNumRows(tstamp, wx) || !SameNumRows(wx, wy) )
		print "IntervalCov: input waves had different lengths - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(wx, p1=lo, p2=hi) || HasNans(wy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] wx, subx
			Duplicate/FREE/R=[lo,hi] wy, suby
			Make/FREE/WAVE/N=2 nanlist = {subx, suby}
			RemoveNansW( nanlist )
			wout[oi] = Cov( subx, suby )
		else
			wout[oi] = Cov( wx, wy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// applies soft spike filter to wave following method of HaPe Schmid with a user-selected interval size; input wave IS modified
//
// 	Schmid, HaPe, C. Susan B. Grimmond, Ford Cropley, Brian Offerle, and Hong-Bing Su. "Measurements 
// 	of CO2 and energy fluxes over a mixed hardwood forest in the mid-western United States." Agricultural 
// 	and Forest Meteorology. 103 (2000): 357-374.
// 		"For each 15min period and variable, the means and variances are calculated. From these diagnostics,
// 		a threshold for spikes is determined as a multiple of the standard deviation (3.6 S.D. initially, increased 
//		by 0.3 after each pass). On each pass, a soft spike is registered if the fluctuation from the mean is larger 
//		than the threshold value, and if the duration of the spike is three or fewer records, corresponding to a 
//		persistence of 0.3s, for the 10Hz sampling rate. Longer-lasting departures from the period mean are taken 
// 		to indicate possible physical events. After each pass, if spikes are detected, the mean and variance are 
//		adjusted to exclude data marked as spikes and the process repeated, until either there are no more new 
//		spikes or the maximum or three iterations is completed (which is rarely the case)."
//
// 2013.05.23 	add `duration` arg to specify max spike duration
// 2011.11.22 	added SameNumRows() check
// 2011.11.21 	split logic to create an Interval_ function and independent function
// 2011.11.18		adapted from legacy function 
Function/WAVE IntervalDespikeHaPe( wname, tstamp, interval, aligned [, multiplier, increment, passes, duration, bp] )
	wave wname							// target wave ref
	wave/D tstamp						// double-precision date/time wave
	variable interval						// size of interval to despike; HaPe used 15min
	variable aligned						// nonzero to align intervals around midnight
	variable multiplier 						// optionally specify different base multipier (def: 3.6)
	variable increment 					// optionally specify different per-pass increment (def: 0.3)
	variable passes 						// optionally specify number of passes (def: 3)
	variable duration 						// optionally specify max duration (secs) that registers as spike (def: 0.3)
	wave bp								// optional, interval boundary points wave as returned by IntervalBoundaries
	
	If ( !SameNumRows(wname, tstamp) )
		print "IntervalDespikeHaPe: timestamp and value waves had different number of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	multiplier = ParamIsDefault(multiplier) ? 3.6 : multiplier
	increment = ParamIsDefault(increment) ? 0.3 : increment
	passes = ParamIsDefault(passes) ? 3 : passes
	duration = ParamIsDefault(duration) ? 0.3 : duration
	
	variable oi, lo, hi, noi = DimSize(bp, 0)
	Make/FREE/N=(noi,passes,2) results = 0
	SetDimLabel 0, -1, output_interval, results
	SetDimLabel 1, -1, pass_number, results
	for (oi=0; oi<passes; oi+=1)
		SetDimLabel 1, oi, $"pass #"+num2istr(oi+1), results
	endfor
	SetDimLabel 2, 0, spikes, results
	SetDimLabel 2, 1, points, results

	for (oi=0; oi<noi; oi+=1) 			// for each subinterval in time series
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			results[oi][][] = NAN
			continue
		endif
		If ( HasNans(wname, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] wname, tmpw
			Duplicate/FREE/R=[lo,hi] tstamp, tmptstamp
			wave that = DespikeHaPe( tmpw, tmptstamp, multiplier=multiplier, increment=increment, passes=passes, duration=duration)
		else
			wave that = DespikeHaPe(wname, tstamp, multiplier=multiplier, increment=increment, passes=passes, duration=duration, p1=lo, p2=hi)
		endif
		results[oi][0, passes-1][%spikes] = that[q][%spikes]
		results[oi][0, passes-1][%points] = that[q][%points]
	endfor			// each subinterval
	
	return results
End	


// returns wave with CO2 flux calculated for each subinterval using eddy covariance
//
// 2013.05.20 	written
Function/WAVE IntervalEC_co2( co2, w_, tstamp, interval, aligned [, bp ] )
	wave co2 			// carbon dioxoide mass density	g / m^3
	wave w_ 			// vertical wind component 			m / s
	wave/D tstamp 		// timestamp						igor date/time
	variable interval		// averaging period				seconds
	variable aligned 		// nonzero to start/stop on whole intervals
	wave bp				// optional interval boundary points wave
	
	If ( !SameNumRows(co2, w_) || !SameNumRows(w_, tstamp) )
		print "IntervalECLatentHeat: input waves had different lengths - aborted"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp, 0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If (HasNans(co2,p1=lo,p2=hi) || HasNans(w_,p1=lo,p2=hi) )
			Duplicate/FREE/R=[lo,hi] co2, subco2
			Duplicate/FREE/R=[lo,hi] w_, subw
			Make/FREE/WAVE/N=2 nanlist = {subco2, subw}
			RemoveNansW( nanlist )
			wout[oi] = EC_co2( subco2, subw )
		else
			wout[oi] = EC_co2( co2, w_, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// TODO
//	review this function for validity
//
// returns wave with latent heat calculated for each subinterval using eddy covariance
//
// 2013.05.31 	FIX: added check that optional argument T_ is also same length as h2o, w_
// 2011.11.22 	changed bp check from ParamIsDefault to !WaveExists; added /D to tstamp param
// 2011.11.14 	written
Function/WAVE IntervalECLatentHeat( h2o, w_, tstamp, interval, aligned [, T_, bp ] )
	wave h2o 			// water vapor molar density		mol / m^3
	wave w_ 			// vertical wind component 			m / s
	wave/D tstamp 		// timestamp						igor date/time
	variable interval		// averaging period				seconds
	variable aligned 		// nonzero to start/stop on whole intervals
	wave T_ 				// optional ambient temp. 			Celcius
		// temp determines latent heat of vaporization as f(meanT each period); uses 2.5e3 J/g otherwise
	wave bp				// optional interval boundary points wave
	
	If ( !SameNumRows(h2o, w_) || !SameNumRows(w_, tstamp) || (WaveExists(T_) && !SameNumRows(w_, T_)))
		print "IntervalECLatentHeat: input waves had different lengths - aborted"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp, 0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If (HasNans(h2o,p1=lo,p2=hi) || HasNans(w_,p1=lo,p2=hi) || (WaveExists(T_) && HasNans(T_,p1=lo,p2=hi)))
			Duplicate/FREE/R=[lo,hi] h2o, subh2o
			Duplicate/FREE/R=[lo,hi] w_, subw
			If ( WaveExists(T_) )
				Duplicate/FREE/R=[lo,hi] T_, subt
				Make/FREE/WAVE/N=3 nanlist = {subh2o, subw, subt}
				RemoveNansW( nanList )
				wout[oi] = ECLatentHeat( subh2o, subw, T_=subt)
			else
				Make/FREE/WAVE/N=2 nanlist = {subh2o, subw}
				RemoveNansW( nanlist )
				wout[oi] = ECLatentHeat( subh2o, subw )
			endif
		elseif ( WaveExists(T_) )
			wout[oi] = ECLatentHeat( h2o, w_, T_=T_, p1=lo, p2=hi )
		else
			wout[oi] = ECLatentHeat( h2o, w_, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// TODO
// 	review function's validity
//
// returns wave with sensible heat calculated for each specified subintervals using eddy covariance
//
// 2013.05.29 	FIX warning about different input wave lengths & include input wave names in feedback
// 2011.11.22 	changed bp check from ParamIsDefault to !WaveExists; added /D flag to tstamp
// 2011.11.14 	written
Function/WAVE IntervalECSensibleHeat( T_, w_, P_, Q_, tstamp, interval, aligned [, bp ] )
	wave T_ 				// ambient temp					Celcius
	wave w_ 			// vertical wind component 			m / s
	wave P_				// ambient pressure 				mbar
	wave Q_ 			// specific humidity				dimensionless, 0-1
	wave/D tstamp 		// timestamp						igor date/time
	variable interval		// averaging period				seconds
	variable aligned 		// nonzero to start/stop on whole intervals
	wave bp				// optional interval boundary points wave
	
	If ( !SameNumRows(T_, w_) || !SameNumRows(w_, tstamp) )
		string nT, nw, nP, nQ
		nT = NameOfWave(T_)
		nw = NameOfWave(w_)
		nP = NameOfWave(P_)
		nQ = NameOfWave(Q_)
		printf "IntervalECSensibleHeat: input waves (%s, %s, %s, %s) had different lengths - aborted\r", nT, nw, nP, nQ
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp, 0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If (HasNans(T_,p1=lo,p2=hi) || HasNans(w_,p1=lo,p2=hi) || HasNans(P_,p1=lo,p2=hi) || HasNans(Q_,p1=lo,p2=hi) )
			Duplicate/FREE/R=[lo,hi] T_, subt
			Duplicate/FREE/R=[lo,hi] w_, subw
			Duplicate/FREE/R=[lo,hi] P_, subp
			Duplicate/FREE/R=[lo,hi] Q_, subq
			Make/FREE/WAVE/N=4 nanlist = {subt, subw, subp, subq}
			RemoveNansW( nanlist )
			wout[oi] = ECSensibleHeat( subt, subw, subp, subq )
		else
			wout[oi] = ECSensibleHeat( T_, w_, P_, Q_, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// TODO
//	verify results of this function
//
// returns 2column wave with eddy covariance density corrections for each subinterval following WPL (80)
//
// 2011.11.22 	written
Function/WAVE IntervalEC_WPL80( rhoC, rhoV, rhoD, T_, w_, tstamp, interval, aligned [, bp] )
	wave rhoC 					// density of constituent C					g / m^3 
	wave rhoV 					// density of water vapor					g / m^3 
	wave rhoD 					// density of dry air						g / m^3 
	wave T_						// ambient temperature 					Celcius
	wave w_ 					// vertical wind component 					m / s
	wave tstamp 					// igor date-time 							seconds
	variable interval 				// size of subintervals						seconds
	variable aligned 				// nonzero to align subintervals to midnight
	wave bp 						// optional point boundary wave returned by IntervalBoundaries
	variable MWc				// optional specify molecular weight of constituent C if using molar density of C
	variable MWv 				// optional specify nonzero if using molar density of H2O (uses MW=18.01528 g/mol)
	variable MWd				// optional specify nonzero if using molar density of dry air (uses MW=28.9645 g/mol)
	
	Make/FREE/WAVE/N=6 wrefs = {rhoC, rhoV, rhoD, T_, w_, tstamp}
	If ( !SameNumRowsW( wrefs ) )
		print "IntervalEC_WPL80: input waves had different numbers of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0),2) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	SetDimLabel 1, 0, h2o_term, wout
	SetDimLabel 1, 1, T_term, wout
	
	variable oi, lo, hi
	variable/C WPL_corr
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi][%h2o_term] = NAN
			wout[oi][%T_term] = NAN
			continue
		endif
		If ( HaveNans( wrefs, p1=lo, p2=hi ) )
			Duplicate/FREE/R=[lo,hi] rhoC, tmpC
			Duplicate/FREE/R=[lo,hi] rhoV, tmpV
			Duplicate/FREE/R=[lo,hi] rhoD, tmpD
			Duplicate/FREE/R=[lo,hi] T_, tmpT
			Duplicate/FREE/R=[lo,hi] w_, tmpW
			Make/FREE/WAVE/N=5 nanlist = {tmpC, tmpV, tmpD, tmpT, tmpW}
			RemoveNansW( nanlist )
			WPL_corr = EC_WPL80_TS( tmpC, tmpV, tmpD, tmpT, tmpW)
		else
			WPL_corr = EC_WPL80_TS( rhoC, rhoV, rhoD, T_, w_, p1=lo, p2=hi )
		endif
		wout[oi][%h2o_term] = real(WPL_corr)
		wout[oi][%T_term] = imag(WPL_corr)
	endfor
	return wout
End


// returns wave with friction velocity calculated for each subinterval
//
// 2012.06.12 	written
Function/WAVE IntervalFrictionVelocity( u_, v_, w_, tstamp, interval, aligned, [bp] )
	wave u_, v_			// horizontal wind components, m/s
	wave w_ 			// vertical wind component, m/s 
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on whole intervals
	wave bp				// optional wave of boundary points from IntervalBoundaries
	
	Make/FREE/WAVE/N=4 wrefs = {u_, v_, w_, tstamp}
	If ( !SameNumRowsW(wrefs) )
		print "IntervalFrictionVelocity: input waves had different numbers of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	variable oi, lo, hi, nancnt
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If (numtype(lo) || numtype(hi))
			wout[oi] = NAN
			continue
		endif
		If ( HaveNans(wrefs, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] u_, subu
			Duplicate/FREE/R=[lo,hi] v_, subv
			Duplicate/FREE/R=[lo,hi] w_, subw
			Make/FREE/WAVE/N=3 nanlist = {subu, subv, subw}
			nancnt = RemoveNansW(nanlist)
			If (nancnt < 0)
				print "uncaught error occured in IntervalFrictionVelocity > RemoveNansW"
			elseif (nancnt > 0)
				print "IntervalFrictionVelocity: removed", nancnt, "NaNs in interval", oi
			endif
			wout[oi] = FrictionVelocity( subu, subv, subw )
		else
			wout[oi] = FrictionVelocity( u_, v_, w_, p1=lo, p2=hi )
		endif
	endfor
	return wout
end


// returns ratio (0-1) of # points missing in each interval
// detects NANs, short intervals and deviations in sampling rate
//
// 2011.11.22 	added SameNumRowsW() check; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>2nd parameter
// 2011.10.27		revised arguments to accept wave of references instead of string list
// 2011.09		initial release
Function/WAVE IntervalMaxPntsGone( wrefs, tstamp, interval, aligned, [bp] )
	wave/WAVE wrefs	// references to waves to check 
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on whole intervals
	wave bp				// optional wave of boundary points from IntervalBoundaries
	
	If ( !SameNumRowsW( wrefs ) )
		print "IntervalMaxPntsGone: referenced input waves had different numbers of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	variable oi, lo, hi, wn, npnts
	Make/FREE/N=(DimSize(bp,0)) pointest
	pointest = bp[p][%hi] - bp[p][%lo] + 1
	npnts = WaveMax(pointest)

	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	wout = 1 - ( (bp[p][%hi] - bp[p][%lo] +1) / npnts )
	
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If (numtype(lo) || numtype(hi))
			wout[oi] = 1
			continue
		endif
		for (wn=0; wn<numpnts(wrefs); wn+=1)
			wave this = wrefs[wn]
			If ( !WaveExists(this) )
				print "IntervalMaxFracGone: could not locate wave <"+NameOfWave(this)+"> - skipping."
				continue
			endif
			wout[oi] = Max( wout[oi], CountNans(this, p1=lo, p2=hi) / (hi - lo + 1) )
		endfor
	endfor
	return wout
end


// calculates the arithmetic mean of each subinterval in <wname>
// 	not affected by NANs in wave
//
// returns wave with results / time set in X scale
//
// 2013.05.29 	modify warning about different wave lengths to include input wave names
// 2011.11.22 	added SameNumRows() check; changed bp check from ParamIsDefault to WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>2nd parameter
// 2011.10.11		removes NANs only if necessary
// 2011.10.06		made insensitive to NANs in <wname>
// 2011.09		initial release
Function/WAVE IntervalMean( wname, tstamp, interval, aligned, [bp] )
	wave wname			// wave of refs to target waves
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of subinterval in seconds
	variable aligned		// nonzero to start/stop on whole multiples of interval
	wave bp				// optional 2D wave with starting/stopping points; see IntervalBoundaries
	
	If ( !SameNumRows( wname, tstamp ) )
		string a_ = NameOfWave(tstamp)
		string b_ = NameOfWave(wname)
		printf "IntervalMean: timestamp (%s) and value (%s) waves had different number of rows - aborting\r", a_, b_
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp, 0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(wname, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] wname, subw
			RemoveNans(subw)
			wout[oi] = mean(subw)
		else
			wout[oi] = mean(wname, pnt2x(wname,lo), pnt2x(wname,hi))
		endif
	endfor
	return wout
End

// returns the monin-obukhov length calculated for each interval
//
// 	uses ObukhovLengthTS() internally
//
// 2011.11.21 	written
Function/WAVE IntervalObukhovLength( u_, v_, w_, Tv, tstamp, interval, aligned [, bp] )
	wave u_, v_				// horizontal wind components 			m / s
	wave w_ 				// vertical wind component				m / s
	wave Tv 					// virtual temp 						Celcius
	wave/D tstamp 			// double precision igor date-time 		seconds
	variable interval 			// size of subintervals					seconds
	variable aligned			// nonzero to align intervals on midnight
	wave bp					// optional wave of boundary points returned by IntervalBoundaries
	
	If ( !SameNumRows(u_,v_) || !SameNumRows(v_,w_) || !SameNumRows(w_,Tv) )
		print "IntervalObukhovLength: input waves have different number of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	
	Make/FREE/N=(DimSize(bp,0)) wout = NAN
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(u_,p1=lo,p2=hi) || HasNans(v_,p1=lo,p2=hi) || HasNans(w_,p1=lo,p2=hi) || HasNans(Tv,p1=lo,p2=hi) )
			Duplicate/FREE/R=[lo,hi] u_, tmpu
			Duplicate/FREE/R=[lo,hi] v_, tmpv
			Duplicate/FREE/R=[lo,hi] w_, tmpw
			Duplicate/FREE/R=[lo,hi] Tv, tmpt
			Make/WAVE/FREE/N=4 nanlist = {tmpu, tmpv, tmpw, tmpt}
			RemoveNansW( nanlist )
			wout[oi] = ObukhovLengthTS( tmpu, tmpv, tmpw, tmpt )
		else
			wout[oi] = ObukhovLengthTS( u_, v_, w_, Tv, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// calculates the standard deviation of each subinterval in <wname>
//	not affected by NANs in wave
// 
// returns wave with results / time set in X scale
//
// 2011.11.22 	added SameNumRows() check and changed bp check from ParamIsDefault to !WaveExists
// 2011.11.21 	added If...else...endif block to only remove NANs when necessary
// 2011.11.16 	moved <tstamp> from 1st>>2nd parameter
// 2011.10.06		made insensitive to NANs in <wname>
// 2011.09		initial release
Function/WAVE IntervalSdev( wname, tstamp, interval, aligned, [bp] )
	wave wname			// wave of refs to target waves
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of subinterval in seconds
	variable aligned		// nonzero to start/stop on whole multiples of interval
	wave bp				// optional 2D wave with starting/stopping points; see IntervalBoundaries
	
	If ( !SameNumRows( wname, tstamp ) )
		print "IntervalSdev: timestamp and value wave had different number of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp, 0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(wname, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] wname, subw
			RemoveNans(subw)
			wout[oi] = sqrt(variance(subw))
		else
			wout[oi] = sqrt(variance(wname, pnt2x(wname,lo), pnt2x(wname,hi)))
		endif
	endfor
	return wout
End


// returns ref to free wave with timestamp values derived from subintervals found in <tstamp>
//
// 2011.11.22 	changed bp check from ParamIsDefault to !WaveExists
// 2011.10.07		unrecognized <edge> values now default to 0
// 2011.09		initial release
Function/WAVE IntervalTimestamps( tstamp, interval, aligned, edge, [bp] )
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of intervals in seconds
	variable aligned		// nonzero to start/stop on multipes of <interval>
	variable edge			// 0: start of interval (default)	1: end	2:midpoint
	wave bp				// optional boundary points from IntervalBoundaries
	
	If ( !WaveExists(bp))
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/D/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	SetScale d, 0, 0, "dat", wout
	
	wout = leftx(bp) + deltax(bp)*p
	switch (edge)
		case 1:
			wout += interval
			break
		case 2:
			wout += 0.5*interval
			break
		default:
			// do nothing
			break
	endswitch
	return wout
End


// returns turbulent kinetic energy in each subinterval derived from orthogonal wind components
//
// 	see TKE(...)
//
// 2011.11.23 	written
Function/WAVE IntervalTKE( u_, v_, w_, tstamp, interval, aligned, [, bp] )
	wave u_, v_, w_ 				// orthogonal wind components 			m / s
	wave tstamp 					// igor date-time						seconds
	variable interval 				// size of subintervals 					seconds
	variable aligned 				// nonzero to align intervals to midnight
	wave bp						// optional boundary points wave
	
	If ( !SameNumRows(u_, v_) || !SameNumRows(v_, w_) )
		print "IntervalTKE: source waves had different number of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(u_,p1=lo,p2=hi) || HasNans(v_,p1=lo,p2=hi) || HasNans(w_,p1=lo,p2=hi) )
			Duplicate/FREE/R=[lo,hi] u_, subu
			Duplicate/FREE/R=[lo,hi] v_, subv
			Duplicate/FREE/R=[lo,hi] w_, subw
			Make/FREE/WAVE/N=3 nanlist = {subu, subv, subw}
			RemoveNansW( nanlist )
			wout[oi] = TKE( subu, subv, subw )
		else
			wout[oi] = TKE( u_, v_, w_, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// modifies <uvwMatrix> by rotating each subinterval using the method of <type>.
// subintervals are of size <interval> and may or may not be <aligned>. 
//
// returns 2D wave with 1st,2nd,3rd rotation angles in cols 0,1,2 (%yaw, %pitch, %roll)
// 	or NAN for errors including bad <type>/<bp> or length of tstamp != length of uvwMatrix
//
// 2011.11.22 	added error msg for !SameNumRows and changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.11.03		added support for planar fit; renamed angles according to rotation axis (yaw, pitch, roll)
// 2011.09		initial release
Function/WAVE IntervaluvwRotation( uvwMatrix, type, tstamp, interval, aligned, [bp])
	wave uvwMatrix					// 2D wave with U,V,W in cols 0,1,2
	variable type						// 0: double-rotation
									// 1: triple rotation
									// 2: planar fit 
	wave/D tstamp					// double precision timestamp wave
	variable interval					// size of subinterval in seconds
	variable aligned					// nonzero start/stop on multiples of <interval>
	wave bp							// optional 2D wave with bounding points as returned by IntervalBoundaries
	
	If ( !SameNumRows(tstamp, uvwMatrix) )
		print "IntervaluvwRotation: timestamp and value matrix had different number of rows - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	variable npnts, oi, lo, hi, p31, p32, p33, psum, sinR, cosR, sinP, cosP, yaw
	npnts = DimSize(bp,0)
	Make/FREE/N=(npnts, 3) rotAngles
	SetDimLabel 1, 0, yaw, rotAngles
	SetDimLabel 1, 1, pitch, rotAngles
	SetDimLabel 1, 2, roll, rotAngles
	SetScale/P x, leftx(bp), deltax(bp), "dat", rotAngles
	SetScale d, 0, 0, "degrees", rotAngles
	
	If ( !type || type==1 )
		for (oi=0; oi<npnts; oi+=1)
			lo = bp[oi][%lo] 						// look up bounding points
			hi = bp[oi][%hi]
			If ( numtype(lo) || numtype(hi) )
				rotAngles[oi][] = NAN
				continue
			endif
			Duplicate/FREE/R=[lo,hi][] uvwMatrix, uvw 
			wave angles = uvwRotation( uvw, type ) 	// do rotation on subinterval
			uvwMatrix[lo, hi][0] = uvw[p - lo][0] 		// dest[lo, hi] = src[0, <#pnts in interval>]
			uvwMatrix[lo, hi][1] = uvw[p - lo][1]
			uvwMatrix[lo, hi][2] = uvw[p - lo][2]
			rotAngles[oi][%yaw] = angles[0][%yaw] 	// save rotation angles
			rotAngles[oi][%pitch] = angles[0][%pitch]
			rotAngles[oi][%roll] = angles[0][%roll]
		endfor
	elseif ( type == 2 )
		// Wilczak, Oncley and Stage. Sonic anemometer tilt correction algorithms. 
		// Boundary-Layer Meteorology 99: 127-150, 2001.
		//
		// 	theory description here
		//
		Duplicate/FREE/R=[][0] uvwMatrix, ux
		Duplicate/FREE/R=[][1] uvwMatrix, uy
		Duplicate/FREE/R=[][2] uvwMatrix, uz
		Redimension/N=(-1,0) ux, uy, uz
		wave u = IntervalMean( tstamp, ux, interval, aligned, bp=bp )
		wave v = IntervalMean( tstamp, uy, interval, aligned, bp=bp )
		wave w = IntervalMean( tstamp, uz, interval, aligned, bp=bp )
		Make/FREE/N=(npnts) u2, v2, w2, uv, uw, vw = NAN
		u2 = u[p]*u[p];	v2 = v[p]*v[p];		w2 = w[p]*w[p]
		uv = u[p]*v[p]; 	uw = u[p]*w[p]; 	vw = v[p]*w[p]
		Make/D/FREE/N=(3,3) eqn = { {npnts,sum(u),sum(v)}, {sum(u),sum(u2),sum(uv)}, {sum(v),sum(uv),sum(v2)} }
		Make/D/FREE/N=(3,1) ans = {sum(w), sum(uw), sum(vw)}
		WAVEclear u, v, w, ux, uy, uz, u,v,w,u2,v2,w2,uv,uw,vw
		
		MatrixOp/FREE coef = Inv(eqn) x ans
		p31 = ( -1*coef[1] ) / sqrt(coef[1]^2 + coef[2]^2 +1)
		p32 = ( -1*coef[2] ) / sqrt(coef[1]^2 + coef[2]^2 +1)
		p33 = 1 / sqrt(coef[1]^2 + coef[2]^2 +1)
		psum = p31^2 + p32^2 + p33^2
		If ( abs(psum-1) > 0.0001 )
			print "IntervaluvwRotation: WARNING orthogonality was not preserved while determing coefficients"
		endif
		sinR = -1*p32 / sqrt( p32^2 + p33^2 )
		cosR = p33 / sqrt( p32^2 + p33^2 )
		sinP = p31
		cosP = sqrt( p32^2 + p33^2 )
		rotAngles[][%roll] = asin(sinR)
		rotAngles[][%pitch] = asin(sinP)
		
		Make/FREE/N=(3,3) C_, D_ = NAN
		C_ = { {1, 0, 0}, {0, cosR, -1*sinR}, {0, sinR, cosR} }
		D_ = { {cosP, 0, sinP}, {0, 1, 0}, {-1*sinP, 0, cosP} }
		MatrixOp/FREE P_ = D_^t x C_^t
		
		for (oi=0; oi<npnts; oi+=1)
			lo = bp[oi][%lo] 
			hi = bp[oi][%hi]
			If ( numtype(lo) || numtype(hi) )
				rotAngles[oi][] = NAN
				continue
			endif
			Duplicate/FREE/R=[lo,hi][] uvwMatrix, uvw
			MatrixOp/FREE uvwMid = ( P_ x uvw^t )^t
			
			Duplicate/FREE/R=[][0] uvwMid, tmpU
			Duplicate/FREE/R=[][1] uvwMid, tmpV
			Redimension/N=(-1,0) tmpU, tmpV
			Make/WAVE/FREE/N=2 nanList = {tmpU, tmpV}
			RemoveNansW( nanList )
			yaw = atan( mean(tmpV) / mean(tmpU) )
			rotAngles[oi][%yaw] = yaw
			
			Make/FREE/N=(3,3) M_ = { {cos(yaw), sin(yaw), 0}, {-1*sin(yaw), cos(yaw), 0}, {0, 0, 1} }
			MatrixOp/FREE uvwFin = ( M_ x uvwMid^t )^t
			uvwMatrix[lo, hi][0] = uvwFin[p - lo][0] 		// dest[lo, hi] = src[0, <#pnts in interval>]
			uvwMatrix[lo, hi][1] = uvwFin[p - lo][1]
			uvwMatrix[lo, hi][2] = uvwFin[p - lo][2]
		endfor
		rotAngles = R2D(rotAngles)
	else
		return NAN
	endif
	return rotAngles
End


// returns reference to free wave containing sdev of WD estimated by Mardia formula
// there is no need for azimuth since only a std dev is returned
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists 
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07		intial release
Function/WAVE IntervalWindDirMardiaSdev( Ux, Uy, tstamp, interval, aligned, [bp] ) 
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindDirMardiaSdev: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			RemoveNansW(nanlist)
			wout[oi] = WindDirMardiaSdev( subx, suby )
		else
			wout[oi] = WindDirMardiaSdev( Ux, Uy, p1=lo, p2=hi )	
		endif
	endfor
	return wout
End


// returns reference to 2-column matrix containing scalar mean (col0 or %WDmean) and std. dev (col1 or %WDsdev) of WD
//
// one way to access the results of this function: 	
//	wave wdtmp = IntervalWindDirScalarMeanSdev( ... )
// 	Duplicate/O/R=[][%WDmean] wdtmp, :$"WD_sclr_avg"
//	Duplicate/O/R=[][%WDsdev] wdtmp, :$"WD_sclr_sdev"
//
// *note: as of 2012/6/12 the above method does not appear to work. try this instead:
//	Duplicate/O IntervalWindDirScalarMeanSdev( ... ), :wd_mean		// copy results to 'real' wave
//	Duplicate/O/R=[][1] wd_mean, :wd_sdev							// copy second column to new wave
//	Redimension/N=(-1,1) wd_mean 								// delete second column from first wave
//
// 2012.06.12 	fixed bug where sdev dimension was set to 0 instead of NaN for missing intervals
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists 
// 2011.11.16 	moved <tstamp> from 1st>>5th parameter
// 2011.10.07		intial release
Function/WAVE IntervalWindDirScalarMeanSdev( Ux, Uy, azimuth, flag, tstamp, interval, aligned, [bp] ) 
	wave Ux, Uy			// horizontal wind components
	variable azimuth		// direction of sonic, degrees east of north
	variable flag			// reserved for sonic type code
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindDirScalarMeanSdev: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0), 2) wout
	SetDimLabel 1, 0, WDmean, wout
	SetDimLabel 1, 1, WDsdev, wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	variable/C result
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi][] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi ) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			RemoveNansW(nanlist)
			result = WindDirScalarMeanSdev( subx, suby, azimuth, flag )
		else
			result = WindDirScalarMeanSdev( Ux, Uy, azimuth, flag, p1=lo, p2=hi )
		endif
		wout[oi][%WDmean] = real( result )
		wout[oi][%WDsdev] = imag( result )
	endfor
	return wout
End


// returns reference to free wave containing unit vector mean wind direction for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>5th parameter
// 2011.10.07		initial release
Function/WAVE IntervalWindDirUnitVectorMean( Ux, Uy, azimuth, flag, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	variable azimuth		// sonic orientation from true north, deg E of N
	variable flag			// reserved for sonic type code
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindDirUnitVectorMean: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			RemoveNansW(nanlist)
			wout[oi] = WindDirUnitVectorMean( subx, suby, azimuth, flag )
		else
			wout[oi] = WindDirUnitVectorMean( Ux, Uy, azimuth, flag, p1=lo, p2=hi )	
		endif
	endfor
	return wout
End


// returns reference to free wave containing resultant mean wind direction for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>5th parameter
// 2011.10.07		intial release
Function/WAVE IntervalWindDirVectorMean( Ux, Uy, azimuth, type, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	variable azimuth		// sonic orientation, deg E of true N
	variable type			// reserved for sonic type code
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindDirVectorMean: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			print "removed", RemoveNansW(nanlist), "nans in interval", oi
			wout[oi] = WindDirVectorMean( subx, suby, azimuth, type )
		else
			wout[oi] = WindDirVectorMean( Ux, Uy, azimuth, type, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave containing sdev of WD by Yamartino formula for each interval
// no azimuth is required since this is just a sdev formula
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07		initial release
Function/WAVE IntervalWindDirYamartinoSdev( Ux, Uy, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindDirYamartinoSdev: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			print "removed", RemoveNansW(nanlist), "nans in interval", oi
			wout[oi] = WindDirYamartinoSdev( subx, suby )
		else
			wout[oi] = WindDirYamartinoSdev( Ux, Uy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave containing scalar mean WS for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07		initial release
Function/WAVE IntervalWindSpeedScalarMean( Ux, Uy, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindSpeedScalarMean: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout

	variable oi, lo, hi	
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			print "removed", RemoveNansW(nanlist), "nans from interval", oi
			wout[oi] = WindSpeedScalarMean( subx, suby )
		else
			wout[oi] = WindSpeedScalarMean( Ux, Uy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave containing scalar harmonic mean WS for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07 	initial release
Function/WAVE IntervalWindSpeedScalarHMean( Ux, Uy, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindSpeedScalarHMean: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout

	variable oi, lo, hi	
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			print "removed", RemoveNansW(nanlist), "nans from interval", oi
			wout[oi] = WindSpeedScalarHMean( subx, suby )
		else
			wout[oi] = WindSpeedScalarHMean( Ux, Uy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave containing std dev of scalar WS for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07		initial release
Function/WAVE IntervalWindSpeedScalarSdev( Ux, Uy, tstamp, interval, aligned, [bp] ) 
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindSpeedScalarSdev: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			RemoveNansW(nanlist)
			wout[oi] = WindSpeedScalarSdev( subx, suby )
		else
			wout[oi] = WindSpeedScalarSdev( Ux, Uy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave with vector mean WS for each interval
//
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.10.07 	function now only removes NANs if some are present (fast, faster)
// 2011.10.06		made insensitive to NANs in <Ux, Uy>
// 2011.09		initial release
Function/WAVE IntervalWindSpeedVectorMean( Ux, Uy, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindSpeedVectorMean: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout
	
	variable oi, lo, hi
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			RemoveNansW(nanlist)
			wout[oi] = WindSpeedVectorMean( subx, suby )
		else
			wout[oi] = WindSpeedVectorMean( Ux, Uy, p1=lo, p2=hi )
		endif
	endfor
	return wout
End


// returns reference to free wave with wind speed persistance calculated for each interval 
//
// 2012.06.13 	*bug fix* inserted RemoveNansW(nanlist) statement
// 2011.11.22 	changed wave length check from points to rows; changed bp check from ParamIsDefault to !WaveExists
// 2011.11.16 	moved <tstamp> from 1st>>3rd parameter
// 2011.09 		written
Function/WAVE IntervalWindSpeedPersist( Ux, Uy, tstamp, interval, aligned, [bp] )
	wave Ux, Uy			// horizontal wind components
	wave/D tstamp		// double precision timestamp wave
	variable interval		// size of interval in seconds
	variable aligned		// nonzero to start/stop on multiples of <interval>
	wave bp				// optional bounding points wave from IntervalBoundaries

	If ( !SameNumRows(tstamp, Ux) || !SameNumRows(Ux, Uy) )
		print "IntervalWindSpeedPersist: source wave lengths are mismatched - aborting"
		return NAN
	elseif ( !WaveExists(bp) )
		wave bp = IntervalBoundaries( tstamp, interval, aligned )
	endif
	Make/FREE/N=(DimSize(bp,0)) wout
	SetScale/P x, leftx(bp), deltax(bp), "dat", wout

	variable oi, lo, hi	
	for (oi=0; oi<DimSize(bp,0); oi+=1)
		lo = bp[oi][%lo]
		hi = bp[oi][%hi]
		If ( numtype(lo) || numtype(hi) )
			wout[oi] = NAN
			continue
		endif
		If ( HasNans(Ux, p1=lo, p2=hi) || HasNans(Uy, p1=lo, p2=hi) )
			Duplicate/FREE/R=[lo,hi] Ux, subx	
			Duplicate/FREE/R=[lo,hi] Uy, suby
			Make/FREE/N=2/WAVE nanlist = {subx, suby}
			print "IntervalWindSpeedPersist: removed", RemoveNansW(nanlist), "NaNs"
			wout[oi] = WindSpeedPersist( subx, suby )
		else
			wout[oi] = WindSpeedPersist( Ux, Uy, p1=lo, p2=hi )	
		endif
	endfor
	return wout
End


// Returns row number of first element in <wname> which violates chronological order or first NAN. If
// ordered chronologically, the result is 0 (false). Special case of wname[0]=NAN return -1
//
// 2011.09		initial release
ThreadSafe Function IsntChronological( wname )
	wave/D wname	// double precision wave to evaluate
	variable i
	
	If (numtype(wname[0])) 
		return -1
	endif
	for (i=1; i<numpnts(wname); i+=1)
		If ( numtype(wname[i]) )
			return i
		elseif ( wname[i-1] >= wname[i] )
			return i
		endif
	endfor
	return 0
End


// returns latent heat of vaporization of water based on temperature
// 	Eqn 8: Henderson-Sellers, B., 1984. A new formula for latent heat of vaporization of water as a function
//			of temperature. Quart. J. R. Met. Soc., 110, pp. 1186-1190
//
// 	Lv = 1.91846e3 * ( T / (T - 33.91) )^2 	where 	[Lv] = J / g
//												[T] = Kelvin
// 2011.11.11 	converted to Celcius
// 2011.11.09 	written
Function LatentHeatVapH2O( T_ )
	variable T_ 		// ambient temp. 				Celcius
	
	return 1.91846e3*( (T_+273.15)/(T_+239.24) )^2 - 273.15
end


// returns distance as-the-crow-flies (Haversine formula) between lat/long pairs 1,2 in meters
// see also: Project
//
// 2011.09		converted interactive function to utility function
// 2010.old		initial release
Function LatLongDistance( lat1, long1, lat2, long2 )
	variable lat1 						// initial point latitude; N=(+), S=(-)
	variable long1					// initial point longitdue; E=(+), W=(-)
	variable lat2						// 2nd point latitude
	variable long2					// 2nd point longitude
	// Sourced from: http://www.movable-type.co.uk/scripts/latlong.html
	// Haversine Formula:			6371 = Earth's mean radius, km
	variable dLat = (lat1-lat2)*(PI/180)		// diff in latitudes, radians
	variable dLong = (long1-long2)*(PI/180)	// diff in longitudes, radians
	variable a = sin(dLat/2)*sin(dLat/2) + cos( lat1*(PI/180) )*cos( lat2*(PI/180) )*sin(dLong/2)*sin(dLong/2)	
	variable c = 2*atan2( sqrt(a), sqrt(1-a) )
	return (6371000*c) 					// distance "as-the-crow-flies" in meters
End


// returns semicolon separated list of all files found in <pathName> optionally searching
// in subfolders too. empty string for none found or invalid path
//
// derived from PrintFoldersAndFiles example under IndexedDir
//
// 2011.09		initial release
Function/S ListFilesIn( pathName, fileFilter, fileExt, recurse, sortBy) // not threadsafe, string
	string pathName			// string name of existing path; use NewPath or Misc->New Path...
	string fileFilter			// reg ex following GrepList to match files names with. "" for all files
	string fileExt				// four character string such as .txt .dat .log ????
	variable recurse			// set max level of recursion; 0 for off, -1 for all levels
	variable sortBy			// -1:modified date, otherwise bitcombo taken from SortList <options> 
							//	nobits	0	default (ascending case-sensitive alphabetical ASCII)
							//	bit0		1	descending sort
							//	bit1		2	numeric sort
							//	bit2		4	case-insensitive sort
							//	bit3		8	case-sensitive alphanumeric using system script
							//	bit4		16	case-insensitive alphanumeric sorting wave9 before wave10
							// valid combos: 0, 1, 2, 3, 4, 5, 8, 9, 16, 17
	
	PathInfo $pathName		// retrieve full directory path
	If ( !V_flag )				// if doesn't exist
		return "" 
	endif
	string prepend = S_path, filesHere, foldersHere, fullList = "", tmpPN, nextDir
	variable i
	
	// retrieve files matching fileFilter reg exp
	filesHere = GrepList(IndexedFile( $pathName, -1, fileExt ), SelectString(strlen(fileFilter), ".", fileFilter))
	If ( sortBy+1 )	// shift truth to preserve SortList options pass-thru
		filesHere = SortList(filesHere, ";", sortBy) 
	endif
	
	for (i=0; i<ItemsInlist(filesHere); i+=1) // prepend each item with full dir string
		fullList += prepend+StringFromList(i, filesHere)+";"
	endfor
	
	if ( recurse )
		foldersHere = IndexedDir( $pathName, -1, 1 )
		If ( sortBy+1 )
			foldersHere = SortList(foldersHere, ";", sortBy)
		endif
		for (i=0; i<ItemsInList(foldersHere); i+=1)
			nextDir = StringFromList(i, foldersHere)
			tmpPN = UniqueName("TmpPath", 12, i)
			NewPath/Q/O $tmpPN, nextDir 
			// decrement <recurse> to halt eventually. initial -1 never becomes false
			fullList += ListFilesIn(tmpPN, fileFilter, fileExt, (recurse-1), sortBy)
			KillPath $tmpPN
		endfor
	endif
	return fullList
End


// recursively searches for folders in <folder> and returns heirachal semicolon separated list
// 
// 2011.10.13		renamed from ListFoldersIn to ListDataFoldersIn
// 2010.old 		initial release
Function/S ListDataFoldersIn( folder )
	string folder						// base folder to search from
	
	If ( !DataFolderExists( folder ) )
		return ""
	endif
	DFREF savDF = GetDataFolderDFR()
	SetDataFolder $folder	
	// make list of folders, strip "FOLDERS:" + trailing "**<CR>", commas->semicolons
	string list=ReplaceString(",",DataFolderDir(1)[8, (strlen(DataFolderDir(1))-3)],";")
	If ( !ItemsInList(list) )
		SetDataFolder savDF	
		return ""	
	else	
		variable i
		string temp, output = ""
		for ( i=0; i<ItemsInList(list); i+= 1)
			temp = StringFromList(i,list)
			If ( DataFolderExists(temp) )
				SetDataFolder $temp	
				output += GetDataFolder(1)+";"	
				SetDataFolder ::	
				output += ListDataFoldersIn(temp)	
			endif
		endfor	
		SetDataFolder savDF	
		return output	
	endif	
End


// loads full file paths from <fileList> according to format <fileType>. If the list has more than one file, a 
// subfolder is created for each one; otherwise, waves are loaded in the current directory. If <overwrite> is
// nonzero, existing subfolders and waves will be replaced. Bit parameter <options> provides fine-grained
// control over subfolder naming, overwriting and permits time stringstamps to be converted into numeric.
//
// 2012.02.28 	fixed output statment: shows correct folder destination for single-file loads
// 2011.11.02 	implemented concatenation operation
// 2011.10.17		removed redundant first bit in options -> shifted bits down
// 2011.09		initial release
Function LoadCSI( fileList, fileType, overwrite, convertTS, concat, options, [baseSFname, B])
	string fileList				// semicolon separated list of full file paths to load
	variable fileType			// pick:	0	TOA5		4-line header
							// 		1	TOACI1		2-line header
							//		2	TOB1		table oriented binary [**not implemented**]
							//		3 	TOB2		table oriented binary [**not implemented**]
							//		4	TOB3		table oriented binary [**not implemented**]
							//	 	5	CSIXML		extensible markup language [**not implemented**]
	variable overwrite			// nonzero to overwrite existing waves and subfolders
							//	applies to loaded waves and, if created, concatenated waves
	variable convertTS		// nonzero to convert string timestamp->Igor date/time; negative to keep source
	variable concat			// nonzero to concatenate sequential files after loading; negative to keep sources
	variable options			// set bit fields for fine control 
		//  bit#  value				-option-	
		//	- 	0		put in subfolder = file's name
		//	0	1		put in subfolder = file's parent folder name 		(overrides bit#1)
		//	1	2		put in subfolder = StringFromMaskedVar(<baseSFname>, # in <fileList>)	
		//	2	4		skip existing subfolders instead of unique naming when <overwrite>=0
		//	3	8		force individual files to load in subfolders as if <fileList> has >=2 files
	string baseSFname		// default: "loadCSI_file\nn"
	string B 					// passed-through to LoadWave as /B=B
	
	variable i, numFiles = ItemsInList(fileList)
	string dfname, fullPath
	DFREF tmpDF, savDF = GetDataFolderDFR()
	Make/DF/FREE/N=(numFiles) destDFRs = NAN
	baseSFname = SelectString(ParamIsDefault(baseSFname), baseSFname, "loadCSI_file\nn")
	B = SelectString( ParamIsDefault(B) || !strlen(B), B, "F=-2, N=timestamp, T=4;") // all CSI files start w/ timestamp column
	
	for ( i=0; i<numFiles; i+=1) 
		fullPath = StringFromList(i, fileList)
		If ( TestBit(options,0) )			// bit#0: file's parent folder as name
			dfname = CleanupName(ParseFilePath(0, fullPath, ":", 1, 1), 0) 
		elseif ( TestBit(options,1) )		// bit#1: use StringFromMaskedVar + # in <fileList>
			dfname = StringFromMaskedVar(baseSFname, i, fixNNwidth=MinFieldWidth(numFiles))
		else 						// default: file's name, no extension
			dfname = CleanupName(ParseFilePath(3, fullPath, ":", 0, 0), 0) 
		endif
		If (DataFolderExists(dfname) && !overwrite && TestBit(options, 2))
			print "LoadCSI: encountered existing subfolder <"+dfname+"> - skipping file "+fullPath
			continue 	// don't waste time loading
		endif
		If (numFiles > 1 || TestBit(options, 3))
			print "LoadCSI: loading <"+fullPath+"> into <"+dfname+">"
		else
			print "LoadCSI: loading <"+fullPath+"> into <"+GetDataFolder(1)+">"
		endif
		
		tmpDF = NewFreeDataFolder()		// work in conflict-free space
		SetDataFolder tmpDF
		switch (fileType)
			case 0: 	// long header, TOA5
				LoadWave/A/B=B/J/K=1/L={1,4,0,0,0}/Q/W fullPath	// took out the /D flag !!!
				break
			case 1: 	// short header, TOACI1
				LoadWave/A/B=B/J/K=1/L={1,2,0,0,0}/Q/W fullPath
				break
			case 2: 	// TOB1
			case 3:	// TOB2
			case 4: 	// TOB3
			case 5: 	// CSIXML
				print "LoadCSI: Unsupported file type specified:", fileType, "- aborting."
				return -1
			default: 
				print "LoadCSI: Invalid file type specified: ", fileType, "- aborting."
				return -1
		endswitch

		If ( convertTS ) 	// convert string->igor date time 
			wave/Z/T ts = timestamp	// explicit name to avoid type ref errors
			If ( !WaveExists(ts) )
				print "LoadCSI: Could not locate <timestamp> after loading file:", fullPath, "- skipping conversion"
			else
				Make/D/FREE/N=(numpnts(ts)) timestamp0
				timestamp0 = string2secs( ts[p], TimeRegEx(0) )
				If ( convertTS < 0 )
					Duplicate/O ts, $"timestamp_STR"
				endif
				KillWaves ts
				Duplicate/O timestamp0, $"timestamp"
				wave timestamp
				SetScale d, 0, 0, "dat", timestamp
			endif
		endif
		
		SetDataFolder savDF
		If (numFiles > 1 || TestBit(options, 3))	
			If (DataFolderExists(dfname))
				If (overwrite)
					KillDataFolder $dfname
				else
					dfname = UniqueName(dfname, 11, 0)
				endif
			endif
			MoveDataFolder tmpDF, :		// drop free into this folder
			RenameDataFolder freeroot, $dfname
			destDFRs[i] = $dfname
		else
			// it is only OK to borrow <i> because it is not needed if numFiles > 1
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
		ConcatAcrossDFRs( destDFRs, "", overwrite, ( concat < 0 ? 0 : 1 ) )
	endif	
	return 0	
End


// loads data files from Los Gatos Research analyzers in similar method of LoadCSI
// 
// 2011.11.28 	added check for 0-length <baseSFname>; made model name case-insensitive and
//				trailing semi-colon tolerant to permit strings from lists; 
// 2011.11.02 	implemented concatenation (NOT TESTED YET)
// 2011.old		initial release
Function LoadLGR( fileList, modelName, overwrite, concat, resamp, options, [baseSFname, B] )
	string fileList				// semicolon separated list of full file paths to load
	string modelName		// specify name of analyzer (spelling critical; not case-sensitive):
								//	"Carbon Dioxide Analyzer"
								//	"Carbon Dioxide Isotope Analyzer"
								//	"Carbon Monoxide Analyzer"
								//	"Economical Ammonia Analyzer"
								//	"Fast Methane Analyzer"
								//	"Greenhouse Gas Analyzer (CH4, CO2, H2O)"
								//	"Hydrogen Flouride (HF) Analyzer"
								//	"Isotopic N2O Analyzer"
								//	"Isotopic Water Analyzer (Liquid+Vapor) - Enhanced Performance"
								//	"Liquid Water Isotope Analyzer"
								//	"Methane Carbon Isotope Analyzer"
								//	"N2O/CO Analyzer"
								//	"NO2 Analyzer (nitrogen dioxide)"
								//	"Water-Vapor Isotope Analyzer"
	variable overwrite			// nonzero to overwrite exisitng waves and subfolders
							//	applies to loaded waves and, if created, concatenated waves
	variable concat			// nonzero to concatnate files sequentially after loading; negative to keep sources
	variable resamp			// specify new rate to resample data files as they are loaded / zero to skip resampling
	variable options			// set bit fields for fine control
		// bit#   value			-option-
		// 	-	0		put in subfolder = file's name 
		// 	0	1		put in subfolder = file's parent folder name 		(overrides bit#1)
		//	1	2		put in subfolder = StringFromMaskedVar(<baseSFname>, # in <fileList>)
		//	2	4		skip existing subfolders instead of unique naming when <overwrite>=0
		//	3	8 		force individual files to load in subfolders as if <fileList> had >=2 files
	string baseSFname		// default: "loadLGR_file\nn"
	string B					// passed-through to LoadWave as /B=B
	
	variable i, j, numFiles = ItemsInList(fileList), numWaves
	string dfname, fullPath, model = RemoveEnding(LowerStr(modelName), ";")
	DFREF tmpDF, savDF = GetDataFolderDFR()
	Make/DF/FREE/N=(numfiles) destDFRs = NAN 
	baseSFname = SelectString(ParamIsDefault(baseSFname) || !strlen(baseSFname), baseSFname, "loadLGR_file\nn")
	B = SelectString( ParamIsDefault(B) || !strlen(B), B, "F=8, N=timestamp, T=4;")
	
	for ( i=0; i<numFiles; i+=1) 									// for each listed file
		fullPath = StringFromList(i, fileList) 
		If ( TestBit(options,0) )			// bit#0: file's parent folder = subfolder name
			dfname = CleanupName(ParseFilePath(0, fullPath, ":", 1, 1), 0)
		elseif ( TestBit(options,1) )		// bit#1: use StringFromMaskedVar + # in <fileList>
			dfname = StringFromMaskedVar(baseSFname, i, fixNNwidth=MinFieldWidth(numFiles))
		else
			dfname = CleanupName(ParseFilePath(3, fullPath, ":", 0, 0), 0)
		endif
		If (DataFolderExists(dfname) && !overwrite && TestBit(options,2))
			print "LoadLGR: encountered existing subfolder <"+dfname+"> - skipping file "+fullPath
			continue 	// skip loading 
		endif
		print "LoadLGR: loading <"+fullPath+"> into <"+dfname+">"
		
		tmpDF = NewFreeDataFolder()
		SetDataFolder tmpDF
		strswitch (model)
			case "carbon dioxide analyzer":
			case "carbon dioxide isotope analyzer":
			case "carbon monoxide analyzer":
			case "economical ammonia analyzer":
			case "fast methane analyzer":
			case "greenhouse gas analyzer (ch4, co2, h2o)":
			case "hydrogen flouride (hf) analyzer":
			case "isotopic n2o analyzer":
			case "isotopic water analyzer (liquid+vapor) - enhanced performance":
			case "liquid water isotope analyzer":
			case "methane carbon isotope analyzer":
				print "LoadLGR: unsupported analyzer specified <"+modelName+"> - aborting."
				return -1
				break
			case "n2o/co analyzer":
				LoadWave/A/B=B/J/K=1/L={1,2,0,0,0}/Q/V={"\t,"," $",0,1}/W fullPath
				break
			case "no2 analyzer (nitrogen dioxide)":
			case "water-vapor isotope analyzer":
			default:
				print "LoadLGR: unsupported analyzer specified <"+modelName+"> - aborting."
				return -1
		endswitch
		
		numWaves = ItemsInlist(S_waveNames)
		If ( !numWaves )
			print "LoadLGR: no waves were loaded from file <"+fullPath+">"
			continue
		endif

		If (resamp)
			WAVE/Z timestamp
			If ( !WaveExists(timestamp) )
				print "LoadLGR: could not locate timestamp after loading file <"+fullPath+"> - skipped resampling."
			else
				wave/WAVE wrefs = WaveList2Refs( S_waveNames, 0 )
				RemoveWaveRef( timestamp, wrefs )
				print "LoadLGR: resampling currently disabled - not resampling waves loaded from <"+fullPath+">"
				//ResampleXY( timestamp, wrefs, resamp )
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
		ConcatAcrossDFRs( destDFRs, "", overwrite, ( concat < 0 ? 0 : 1 ) )
	endif	
	return 0
End


// 2013.02.18 	fix errors in timestamp conversion
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
				Make/D/N=(numpnts(datew)) timestamp = datew[p] + timew[p]
				If ( tsconv > 0 )
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


// returns minimum # of fields necessary to hold absolute value of integer <num> 
// 	MinFieldWidth( 0 ) = 0				MinFieldWidth( -2 ) = 1
//	MinFieldWidth( 0.5 ) = 0			MinFieldWidth( 1000 ) = 4
//	MinFieldWidth( 2 ) = 1 			MinFieldWidth( -99999.9 ) = 5
//
// 2011.09		initial release
ThreadSafe Function MinFieldWidth( num )
	variable num
	if (num)
		return (floor(log(abs(num)))+1)
	else
		return 0		// return 0 for special case num=0
	endif
End


// returns dimensionless molar mixing ratio of constituent C == ratio of moles of C to moles of dry air
// use optional parameter <inMass> to get a mass mixing ratio
//
// when using the virtual temp (or the sonic temp. as an approximation thereof) then h2o does not need
// to be specified--just provide zero
//
//	https://secure.wikimedia.org/wikipedia/en/wiki/Mixing_ratio
// 	http://amsglossary.allenpress.com/glossary/search?id=mixing-ratio1
//
// 	Derivations based on the ideal gas law:	PV = nRT 
//		Xc = (n/V)C / [ (P/RT)air - (n/V)h2o ]		(n/V)C = constituent molar density	mol / m^3
//												P = barometric pressure		mbar
//												R = univ. gas constant		(mbar m^3)/(mol K)
//												T = ambient temp. 			Celcius
//											(n/V)h2o = H2O molar density 		mol / m^3
//
// 	Derivations based on the ideal gas law:	PV = n(R"/MW)T 
//		Xc = (m/V)C / [ (P/R"T)air - (m/V)h2o ] 	(m/V)C = constituent mass density	g / m^3
//												R" = dry air gas constant		(mbar m^3)/(g K)
//											(m/V)h2o = H2O mass density 		g / m^3
//
// 2011.11.15		assembled from prototypes
Function MixingRatio( C_, T_, P_, h2o [, inMass] )
	variable C_ 			// trace gas molar density		mol / m^3 	OR 		g / m^3
	variable T_			// ambient temp				Celcius
	variable P_			// barometric press.			mbar = hPa 	(1kPa = 10hPa)
	variable h2o 			// H2O molar density			mol / m^3 	OR 		g / m^3
	variable inMass		// nonzero to use mass units
	If ( inMass )
		return C_ / ( DensityOfAir(T_, P_) - h2o )
	else
		return C_ / ( DensityOfAir(T_, P_, inMoles=1) - h2o )
	endif
End


// returns dimensionless molar mixing ratio of constituent C == ratio of moles of C to moles of dry air
//
//	Personal communication with Licor engineer in the fall of 2011 - Patrick O'Keeffe
//	Yes, a better source should be found.
// 
//	Xc = MFc / (1-MFw)					MFc = constituent mole fraction	dimensionless
//										MFw = H2O mole fraction			dimensionless
//
// 2011.11.15 	added mixing ratio parameter 
// 2011.11.14	 	adjusted to Celcius input
// 2011.11.08 	written
Function MixingRatioMF( C_, T_, P_, MFw )
	variable C_ 			// trace gas molar density		mol/m^3 = (mass/molecular weight) / m^3
	variable T_			// ambient temp				Celcius
	variable P_			// barometric press.			mbar = hPa
	variable MFw 		// H2O mole fraction			dimensionless
	return MoleFraction(C_, T_, P_) / (1 - MFw)
End


// returns dimensionless mass mixing ratio of a vapor == ratio of mass of vapor to mass of dry air
// default vapor is H2O; use optional parameter <MW> to specify molecular weight of a different vapor if desired
//
// 	http://amsglossary.allenpress.com/glossary/search?id=mixing-ratio1
// 		R = (MWv / MWd)*e / (P - e)		e,P = vapor, barometric pressures		same units
//										MWv / MWd = Rd/Rv 	 				dimensionless
//
// 2011.11.15 	generalized to handle any vapor
// 2011.11.08 	written
Function MixingRatioVP( e_, P_ [, MW] )
	variable e_			// vapor pressure							same units as P_
	variable P_			// barometric pressure						same units as e_
	variable MW			// optional specify different MW than H2O	g / mol
						// 	OR specify MW=0 to get results in molar units
	MW = ParamIsDefault(MW) ? kMW_h2o : MW
	If ( MW )
		return (MW / kMW_dryair)*e_ / (P_ - e_) 
	else
		return e_ / (P_ - e_)
	endif
End


// Returns inVal after adding/subtracting 360 to bring within range 0 <= X < 360
//
// 2010.old		initial release
ThreadSafe Function ModWD( inVal )
	variable inVal							// value to adjust
	do 									// begin forever loop
		if ( inVal < 0 ) 						// if value is negative
			inVal += 360 						// add 360
		elseif ( inVal >= 360 ) 					// if value is >360
			inVal -= 360 							// subtract 360
		else 								// otherwise
			return inVal 							// quit, return adj. value
		endif
	while (1)
End


// returns dimensionless mole fraction of constituent C == ratio of moles of C to total moles in system
// 
//	M == (C, mol/m^3) / (ambient air, mol/m^3) 			C = constituent molar density		mol / m^3
//		= C / (n/V = P/RT)air							R = univ. gas constant 			(mb m^3)/(mol K)
//		= C * (RT/P)air 								P = barometric pressure			mbar
//													T = ambient temp.				Celcius
//
// 2011.11.08 	written
Function MoleFraction( C_, T_, P_ )
	variable C_			// trace gas molar density 		mol/m^3 = (mass/molecular weight) / m^3
	variable T_			// ambient abs. temp. 			Kelvin
	variable P_ 			// barometric press. 			mbar = hPa	
	return C_ * kRu * (T_+273.15) / P_
End


// Returns a DFREF which can be used to switching or referring to
//
// 2011.old		initial release
Function/DF NewDataFolderX( destpath ) // not threadsafe, df
	string destPath		// relative or absolute path of directory to create
	variable i	
	string folder = ""
	string up = ""
	DFREF sav0 = GetDataFolderDFR()
	DFREF out	
	for (i=0; i<ItemsInList(destPath, ":"); i+=1) 						// for each folder in path
		folder = ReplaceString("'", StringFromList(i, destPath, ":"), "")		// single quotes screw up NewDatafolder
		If ( !strlen(folder) ) 											// if blank entry (found semicolon group)
			up += SelectString(i, "", ":")									// +semicolon, except for first row
			continue														// skip remainder of loop
		elseif (strlen(up))												// if not blank but last entry was
			up += ":" 													// grouped :'s lose the first ":" so replace it
			up = up[0, min(strlen(up), ItemsInList(GetDataFolder(1), ":"))-1] 	// remove :'s which refer upwards of root
			SetDataFolder $up											// switch up levels
		endif
		If (cmpstr(folder,"root"))										// if folder isn't "root"
			NewDataFolder/O/S $folder 									// create/switch to
		else 														// but if it is
			SetDataFolder root:											// start in root, don't try to make it
		endif
		up = ""														// clear up before next loop
	endfor
	out = GetDataFolderDFR() 									// capture current location to DFREF first
	SetDataFolder sav0			 								// then return user to prev folder
	return out				 									// and hand back the DFREF
end


// TODO
//	make new window autoposition below existing windows for nested calls
//
// creates a new progress window with 0% progress and returns name of the window
//
// 2011.10.28 	initial working version
Function/S NewProgressWindow()
	string myname = UniqueName("progwin", 9, 0)
	STRUCT rect c	// coordinates
	c.left=629; c.top=345; c.right=929; c.bottom=442

	NewPanel/K=1/FLT=1/N=$myname/W=(c.left,c.top,c.right,c.bottom) as "Please wait..."
	ValDisplay vdProgress,pos={5,5},size={290,20},limits={0,1,0},barmisc={0,0},mode= 3,value= _NUM:0
	TitleBox titleProgress,pos={5,30},size={54,13},frame=0,title="0% complete"
	Button btnAbort,pos={245,26},size={50,20},help={"Click to end procedure execution"},title="Abort"
	TitleBox titleMsg,pos={5,50},size={290,42},fixedSize=1,title=" "
	SetActiveSubwindow _endfloat_
	DoUpdate/W=$myname/E=1
	return myname
End

// returns the monin-obukhov length (meters) from variables 
//
// 	http://amsglossary.allenpress.com/glossary/search?id=monin-obukhov-similarity-theory1
//	http://amsglossary.allenpress.com/glossary/search?id=obukhov-length1
// 		L = [ -1 * <Tv> * (u*)^3 ] / [ k * G * Cov(w, Tv) ]		u* = friction velocity			m / s
//														Tv = virtual temp				Kelvin
//			<...> denotes averaging						k = von Karman's constant 	dimensionless
//														G = gravity force				m / s^2
//														w = vertical wind component 	m / s
// 	https://secure.wikimedia.org/wikipedia/en/wiki/Monin-Obukhov_Length
//		provides an identical definition, albiet with the virtual _potential_ temp
// 
// 2011.11.22 	derived from ObukhovLength()
Function ObukhovLength( frictionVelocity, meanTv, cov_w_Tv )
	variable frictionVelocity		// friction velocity								m / s
	variable meanTv 				// mean virtual temperature						Celcius
	variable cov_w_Tv				// covariance of vertical wind and virtual temp		C m / s = K m / s
	
	return ( -1* (meanTv+273.15) * frictionVelocity^3 ) / ( kVonKarman * kGravity * cov_w_Tv )
End

// returns the monin-obukhov length (meters) from time series
//
// 	http://amsglossary.allenpress.com/glossary/search?id=monin-obukhov-similarity-theory1
//	http://amsglossary.allenpress.com/glossary/search?id=obukhov-length1
// 		L = [ -1 * <Tv> * (u*)^3 ] / [ k * G * Cov(w, Tv) ]		u* = friction velocity			m / s
//														Tv = virtual temp				Kelvin
//			<...> denotes averaging						k = von Karman's constant 	dimensionless
//														G = gravity force				m / s^2
//														w = vertical wind component 	m / s
//
// 2011.11.22 	extracted final logic to ObukhovLength()
// 2011.11.21 	written
Function ObukhovLengthTS( u_, v_, w_, Tv [, p1, p2] )
	wave u_, v_	 			// horizontal wind components 			m / s
	wave w_ 				// vertical wind component 				m / s
	wave Tv 					// virtual temp						Celcius
	variable p1, p2 			// optional point boundaries
	
	If ( !SameNumPnts(u_,v_) || !SameNumPnts(v_,w_) || !SameNumPnts(w_,Tv) )
		print "ObukhovLengthTS: input waves were not the same length - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(u_,0)-1 : p2), p1, DimSize(u_,0)-1 )	
	
	variable ustar, mTv, cov_w_Tv
	ustar = FrictionVelocity( u_, v_, w_, p1=p1, p2=p2 )
	mTv = mean( Tv, pnt2x(Tv,p1), pnt2x(Tv,p2) )
	cov_w_Tv = Cov( w_, Tv, p1=p1, p2=p2 )
	return ObukhovLength( ustar, mTv, cov_w_Tv )
End


// returns potential temperature, the temp. an air parcel would have if it were expanded or
// compressed adiabatically from its existing T/P to a standard pressure, P0 = 1000mb
//
// 	Arya, S Pal. Introduction to Micrometeorology. 2nd Ed. 2001. Academic Press.
//		5.7		Tp = T*(1000/P)^k				T,P = ambient temp, press			Kelvin, mbar
//											k = R/Cp ~= 0.286
//	
// 	Atmospheric Science An Introductory Survey by John Wallace and Peter Hobbs.
//	Academic Press, 1977. ISBN 0-12-732950-1
//		2.57		Tp = T*(P0/P)^(R/Cp) 			T = ambient temp					Kelvin
//											P0,P = reference,ambient press 	mbar
//											R/Cp ~= 0.286
//
// 2011.11.15		added reference pressure as optional parameter
// 2011.11.04 	written
Function PotentialTemp( T_, P_ [, P0 ] )
	variable T_			// ambient abs. temp.				Kelvin
	variable P_			// ambient barometric pressure		mbar
	variable P0			// optional reference pressure		mbar	assumed 1000mb

	P0 = (ParamIsDefault(P0) || numtype(P0)) ? 1000 : P0
	return T_ * (P0 / P_)^(kR_dryair / kCp_dryair)
End


// Prompts user to select datafolder, switches to it and returns a
// DFREF to /WHERE/ the user /WAS/ at
//
// 2011.old		initial release
Function/DF PromptSetDataFolder() // not threadsafe, df
	DFREF sav0 = GetDataFolderDFR()
	string fname = GetDataFolder(1) 
	Prompt fname, "Select datafolder containing waves:", popup, GetDataFolderList() 
	do 	
		DoPrompt "Data folder selection", fname 
		If (V_flag) 
			return NAN 
		elseif ( DataFolderExists(fname) )
			break; 
		endif
	while (1)														// continue loop
	SetDataFolder $fname											// switch folders
	return sav0
End


// Returns semicolon separated list of full file paths; empty list if user cancels
//
// 2011.old		initial release
Function/S PromptForFileList( msg ) // not threadsafe, string
	string msg
	variable refnum
	
	Open/D/R/F=ksFileFilter/M=(msg)/MULT=1 refnum		// ask user to pick files
	If ( strlen(S_fileName) == 0 )
		return ""
	else
		// replace <CR> delimiters with ";" and sort case-sensitive alphabetic
		return SortList(ReplaceString(num2char(13), S_fileName, ";"))
	endif
end


// returns <inVal> after converting from radians to degrees
//
// 2010.old		initial release
ThreadSafe Function R2D( inVal )
	variable inVal
	return inVal*(180/PI)
End


// returns relative humidity based on ambient temperature and H2O vapor pressure
//
// 2011.11.08 	written
Function RelativeHumidity( e_, T_ )
	variable e_ 		// H2O vapor pressure 	mbar = hPa
	variable T_ 		// ambient temp. 			Celcius
	
	return e_ / SatVP( T_ ) 	// mbar
End


// removes all blank points in <theWave> and returns # of blanks removed
//
// this function was inspired by the RemoveNaNs function in <Remove Points>
//
// 2010.old		initial release
ThreadSafe Function RemoveBlanks(theWave)
	WAVE/T theWave
	variable p = 0	
	variable blanks = 0
	string str
	do												// do
		str = theWave[p]									// retrieve value
		if ( strlen(str) <= 0 )								// is this blank?
			blanks += 1										// add to counter
		else												// if not blank
			theWave[p - blanks] = str							// shift record backwards, overwriting blanks
		endif												
		p += 1											// next row
	while (p < numpnts(theWave))						// until end of wave
	DeletePoints numpnts(theWave)-blanks, blanks, theWave 	// truncate the wave
	return blanks
End


// Removes NANs from waves while preserving correct alignment of original values. 
// Checks each wave for NANs and if one is found, that row is deleted from each wave in wrefs.
//
// Returns 	variable 	total # of NANs removed
//			-1 if duplicate wave reference
//			-2 if waves were not all the same length 
//
// 2010.old		initial release
Function RemoveNansW( wrefs )
	wave/WAVE wrefs 							// wave of references to affect
	
	variable i, j, k									// counters
	variable nans, rows 							// nans/loop, total rows removed
	
	If ( HasDuplicateWRefs(wrefs) )
		return -1
	endif
	If ( !SameNumPntsW(wrefs) )
		return -2
	endif
	
	for (i=0; i<numpnts(wrefs); i+=1)
		wave foo = wrefs[i] 
		nans = 0 									// reset counter
		for (j=0; j<numpnts(foo); j+=1) 					// for each point in this wave
			if (numtype(foo[j])==2) 						// if a nan
				nans += 1 									// tally
			elseif ( !nans )								// if no nans yet
				continue										// skip to next point
			else 										// if some nans were found
				for (k=0; k<numpnts(wrefs); k+=1) 				// for each wave in list
					wave bar = wrefs[k]							// grab ref
					bar[j - nans] = bar[j] 							// shift record back over nans
				endfor
			endif
		endfor
		rows += nans 								// add counter to total
		for (j=0; j<numpnts(wrefs); j+=1) 				// for each wave in list
			wave baz = wrefs[j] 							// make ref
			DeletePoints (numpnts(baz)-nans), nans, baz 	// delete rows at end where NANs were shifted to
		endfor
	endfor
	return rows
End


// removes any references in <wrefs> which are equivalent to <remref>
Function RemoveWaveRef( remref, wrefs )
	wave remref
	wave/WAVE wrefs
	variable i
	for (i=0; i<numpnts(wrefs); i+=1)
		wave this = wrefs[i]
		If ( WaveRefsEqual(remref, this) )
			DeletePoints i, 1, wrefs
			i -= 1
		endif
	endfor
End


// whenever comparison described by <mode/val1/val2> is true, corresponding rows from waves in <wrefs> are set
// to <withVal>, which may be NAN. 
//
// text & multidimensional waves are ignored
// 
// 2011.11.01 	initial remodelling of lar_nanFill(...)
Function ReplaceWaveValues(wrefs, withVal, mode, val1, val2, [p1, p2])
	wave/WAVE wrefs		// wave of references to target waves
	variable withVal			// new value to insert 
	variable mode			// bit-map combination of possible expressions
			// 	bit 	val
			//	--	0 	all points
			// 	0	1	equal to <val1> / also sets comparisons to inclusive 
			//	1	2	less than <val1>
			// 	2	4 	between <val1> and <val2> / window comparison
			//	3 	8 	within <val2> OF <val1> / tolerance comparison
			//	4		reverse effect of bit 0 when bit 0 is used alone 				not equal to <val1>
			//	5		reverse effect of bit 1 when bit 1 is used alone or with bit 0 	greater than <val1>
			// 	6 		reverse effect of bit 2 when bit 2 is used alone or with bit 0 	outside <val1> and <val2>
			//	7		reverse effect of bit 3 when bit 3 is used alone or with bit 0 	not within <val2> OF <val1>
			// Bits 1-3 may be combined with bit 0 to perform inclusive comparisons but they are mutually exclusive 
			//	to each other with precedence given to the highest bit #.
			// Bit 4 only has an effect when combined with bit0 and no other bits
			// Bits 5-6 may be combined with bits 1-3 respectively to perform the complementary comparison
	variable val1				// comparison value
	variable val2				// auxilary comparison value
	variable p1				// 	\ optional point-range filters, inclusive
	variable p2				// 	/ default to full-range
	
	If ( !SameNumPntsW(wrefs) )
		DoAlert 1, "Not all these waves are the same length. Continue replacing values?"
		If (V_flag == 2)
			return -1
		endif
	endif

	variable i, lo, hi
	for (i=0; i<numpnts(wrefs); i+=1)
		wave/Z this = wrefs[i] 
		If ( !WaveExists(this) )
			print "ReplaceWaveValues: couldn't find wave <"+NameOfWave(this)+"> - skipping"
			continue
		elseif ( WaveDims(this) != 1 )
			print "ReplaceWaveValues: wave <"+NameOfWave(this)+"> is multidimensional - skipping"
			continue
		elseif ( mode && WaveType(this, 1)==2 )
			print "ReplaceWaveValues: cannot perform comparisons on text wave <"+NameOfWave(this)+"> - skipping"
			continue
		endif
		lo = Limit(p1, 0, p1)
		hi = Limit( (ParamIsDefault(p2) ? numpnts(this)-1 : p2), p1, numpnts(this)-1)
		If ( mode )
			If ( TestBit(mode, 0) )						// equals/inclusive branch
				If ( TestBit(mode,3) )						// tolerance
					If ( TestBit(mode,7) )						// [X] <= (val1 - val2) || (val1 + val2) <= [X]
						this[lo,hi] = ( abs(this[p]-val1) >= abs(val2) ) ? withVal : this[p] 
					else										// (val1 - val2) <= [X] <= (val1 + val2)
						this[lo,hi] = ( abs(this[p]-val1) <= abs(val2) ) ? withVal : this[p] 
					endif
				elseif ( TestBit(mode,2) )					// window
					If ( TestBit(mode,6) )						// [X] <= val1 || val2 <= [X]
						this[lo,hi] = ( this[p] <= val1 || val2 <= this[p] ) ? withVal : this[p]
					else 									// val1 <= [X] <= val2 
						this[lo,hi] = ( val1 <= this[p] && this[p] <= val2 ) ? withVal : this[p]
					endif
				elseif ( TestBit(mode,1) )					// greater/less than
					If ( TestBit(mode,5) )						// val1 <= [X]
						this[lo,hi] = ( val1 <= this[p] ) ? withVal : this[p]
					else 									// [X] <= val1
						this[lo,hi] = ( this[p] <= val1 ) ? withVal : this[p]
					endif
				else 									// equal/not equal to
					If ( TestBit(mode,4) )						// [X] != val1
						this[lo,hi] = ( this[p] != val1 ) ? withVal : this[p]
					else 									// [X] == val1
						this[lo,hi] = ( this[p] == val1 ) ? withVal : this[p]
					endif
				endif
			else 									// exclusive branch
				If ( TestBit(mode,3) )						// tolerance
					If ( TestBit(mode,7) )						// [X] < (val1 - val2) || (val1 + val2) < [X]
						this[lo,hi] = ( abs(this[p]-val1) > abs(val2) ? withVal : this[p] )
					else										// (val1 - val2) < [X] < (val1 + val2)
						this[lo,hi] = ( abs(this[p]-val1) < abs(val2) ? withVal : this[p] )
					endif
				elseif ( TestBit(mode,2) )					// window
					If ( TestBit(mode,6) )						// [X] < val1 || val2 < [X]
						this[lo,hi] = ( this[p] < val1 || val2 < this[p] ) ? withVal : this[p]
					else 									// val1 < [X] < val2 
						this[lo,hi] = ( val1 < this[p] && this[p] < val2 ) ? withVal : this[p]
					endif
				elseif ( TestBit(mode,1) )					// greater/less than
					If ( TestBit(mode,5) )						// val < [X]
						this[lo,hi] = ( val1 < this[p] ) ? withVal : this[p]
					else 									// [X] < val1
						this[lo,hi] = ( this[p] < val1 ) ? withVal : this[p]
					endif
				endif
			endif
		else
			If ( !lo && hi == numpnts(this) )
				string msg = "It appears all points in wave <"+NameOfWave(this)+"> will be set to "+num2str(withVal)
				msg += "\r\rPress yes to continue, no to skip or cancel to quit"
				DoAlert 2, msg 
				If ( V_flag == 2 )
					continue
				elseif ( V_flag == 3)
					return -1
				endif
			endif
			this[lo,hi] = withVal
		endif
	endfor
	return 0
End

// TODO
//	improve how this function returns results
//
// resamples each wave in <wrefs> at <newRate> in seconds; 
//
// create free wave of wave refs on command line using {} like so:
// 	ResampleXY( timestamp, {singlewave}, newRate )
//
// 2011.09-10 	initial implementation
Function ResampleXY( tstamp, wrefs , newRate )
	wave/D tstamp 			// double precision timestamp wave 
	wave/WAVE wrefs 		// wave references to resample; do not include timestamp
	variable newRate 			// new sample interval in seconds
	
	variable srcpnts, destpnts, rndcol, loTimeOff, hiTimeOff, i
	string yname, xname = NameOfWave(tstamp)+"_RS"
	
	srcpnts = numpnts(tstamp)
	destpnts = BankerRound( (tstamp[srcpnts] - tstamp[0])*(1/newRate)+1, 0)
	rndcol = floor(log(newRate))
	
	// shift timestamps so first or last record is at whole interval of <newRate>; choose the smaller shift;
	//	ex1: time of first record is 08:05:16.327; subtract 0.027 from every timestamp 
	//	ex2: time of last record is 19:32:40.889; add 0.011 to every timestamp
	// round opposing end value to nearest whole interval	
	//	ex1: shifted last record is 20:08:30.675; round to 20:08:30.7
	//	ex2: shifted first record is 06:13:27.310; round to 06:13:27.3
	Make/D/FREE/N=(srcpnts) tstampAdj = tstamp
	loTimeOff = BankerRound( tstamp[0], rndcol ) - tstamp[0]
	hiTimeOff = BankerRound( tstamp[srcpnts], rndcol ) - tstamp[srcpnts]
	If ( Abs(loTimeOff) < Abs(hiTimeOff) )
		tstampAdj += loTimeOff
		tstampAdj[srcpnts] = BankerRound( tstampAdj[srcpnts], rndcol )
	else
		tstampAdj += hiTimeOff
		tstampAdj[0] = BankerRound( tstampAdj[0], rndcol )
	endif

	for (i=0; i<numpnts(wrefs); i+=1)
		wave this = wrefs[i]
		print "\tResampling <"+NameOfWave(this)+">"
		yname = NameOfWave(this)+"_RS"
		Interpolate2/T=3/F=0/N=(destpnts)/X=$xname/Y=$yname tstampAdj, this
		wave xwave = $xname
		wave ywave = $yname
		xwave = BankerRound( xwave[p], rndcol ) // even resampled, X-spacing isn't exact enough for FFT, etc -> round
		SetScale d, 0, 0, "dat", xwave
		SetScale/P x, xwave[0], (xwave[1] - xwave[0]), "dat", xwave, ywave
	endfor
End


// Returns truth of whether <w1> and <w2> have == number of columns
//
// 2011.old		initial release
ThreadSafe Function SameNumCols( w1, w2 )
	wave w1, w2
	return (DimSize(w1, 1) == DimSize(w2, 1))
end


// Returns truth of whether waves referenced in <wrefs> have == number of columns
//
// 2011.11.22		derived from SameNumPntsW
ThreadSafe Function SameNumColsW( wrefs )
	wave/WAVE wrefs
	variable i
	for (i=0; i<DimSize(wrefs,0)-1; i+=1)
		If ( DimSize(wrefs[i],1) != DimSize(wrefs[i+1],1) )
			return 0
		endif
	endfor
	return 1
End


// Returns truth of whether <w1> and <w2> have == number of chunks
//
// 2011.old		initial release
ThreadSafe Function SameNumChunks( w1, w2 )
	wave w1, w2
	return (DimSize(w1, 3) == DimSize(w2, 3))
end


// Returns truth of whether waves referenced in <wrefs> have == number of layers
//
// 2011.11.22		derived from SameNumPntsW
ThreadSafe Function SameNumChunksW( wrefs )
	wave/WAVE wrefs
	variable i
	for (i=0; i<DimSize(wrefs,0)-1; i+=1)
		If ( DimSize(wrefs[i],3) != DimSize(wrefs[i+1],3) )
			return 0
		endif
	endfor
	return 1
End


// Returns truth of whether <w1> and <w2> have == number of layers
//
// 2011.old		initial release
ThreadSafe Function SameNumLayers( w1, w2 )
	wave w1, w2
	return (DimSize(w1, 2) == DimSize(w2, 2))
end


// Returns truth of whether waves referenced in <wrefs> have == number of layers
//
// 2011.11.22		derived from SameNumPntsW
ThreadSafe Function SameNumLayersW( wrefs )
	wave/WAVE wrefs
	variable i
	for (i=0; i<DimSize(wrefs,0)-1; i+=1)
		If ( DimSize(wrefs[i],2) != DimSize(wrefs[i+1],2) )
			return 0
		endif
	endfor
	return 1
End


// Returns truth of whether <w1> and <w2> have == number of points
//
// 2010.old		initial release
ThreadSafe Function SameNumPnts( w1, w2 )
	wave w1, w2
	return (numpnts(w1) == numpnts(w2))
end


// returns truth of whether all waves referenced in <wrefs> have the same number of points
//
// 2011.11.22 	changed i-loop limit from numpnts(wrefs) to DimSize()
// 2011.11.21 	inverted logic sense; renamed from AreNotSameLength
// 2011.11.17 	renamed from NotAllSameLength
// 2011.old		initial release
ThreadSafe Function SameNumPntsW( wrefs )
	wave/WAVE wrefs
	variable i
	for (i=0; i<DimSize(wrefs,0)-1; i+=1)
		If (numpnts(wrefs[i]) != numpnts(wrefs[i+1]))
			return 0
		endif
	endfor
	return 1
End


// Returns truth of whether <w1> and <w2> have == number of rows
//
// 2011.old		initial release
ThreadSafe Function SameNumRows( w1, w2 )
	wave w1, w2
	return (DimSize(w1, 0) == DimSize(w2, 0))
end


// Returns truth of whether waves referenced in <wrefs> have == number of rows
//
// 2011.11.22		derived from SameNumPntsW
ThreadSafe Function SameNumRowsW( wrefs )
	wave/WAVE wrefs
	variable i
	for (i=0; i<DimSize(wrefs,0)-1; i+=1)
		If ( DimSize(wrefs[i],0) != DimSize(wrefs[i+1],0) )
			return 0
		endif
	endfor
	return 1
End


// Returns truth of wheter x-scaling is identical between <w1> and <w2>
//
// 2010.old		initial release
ThreadSafe Function SameXscale( w1, w2 )
	wave w1, w2
	return ( leftx(w1)==leftx(w2) && deltax(w1)==deltax(w2) )
end


// returns saturation vapor pressure based on ambient temperature
// 	
//	http://amsglossary.allenpress.com/glossary/search?id=clausius-clapeyron-equation1
//	Bolton, D., 1980: The Computation of Equivalent Potential Temperature. Monthly Weather
//	Review. Vol 108, 1046-1053. 
//		Eqn 10: 		es(T) = 6.112*exp( (17.67*T) / (T+243.5) ) 		T = ambient temp, Celcius
//
// 2011.11.08 	written
Function SatVP( T_ )
	variable T_		// ambient temp. 			Celcius
	
	return 6.112*exp( (17.67*T_) / (T+243.5) ) 	// mbar
End


// Returns <var> with bit # <bit> set to 1
//
// derived directly from the demo under Using BitWise Operators
//
// 2011.09		initial release
ThreadSafe Function SetBit( var, bit )
	variable var, bit
	return ( trunc(var) | (2^bit) )
End


// Returns <var> after shifting bits to the left (+) or right (-) <where> number of places
//
// derived directly from the demo under Using Bitwise Operators
//
// 2011.09		initial release
ThreadSafe Function ShiftBit( var, by )
	variable var, by
	if ( sign(by) + 1 ) // true for positive where
		return ( trunc(var) * (2^by) )	// left shift
	else		// negative where
		return ( trunc(var) / (2^abs(by)) )	// right shift
	endif
End	


// returns dimensionless specific humidity, defined as mass of water vapor to total mass of the system
//
//	http://amsglossary.allenpress.com/glossary/search?id=specific-humidity1
//		Q 	= Mw / (Mw+Md) 				Mw,Md = mass of water vapor,dry air 	dimensionless
//			= [Mw / (Mw+Md)]*[Md/Md]
//			= (Mw/Md) / (Mw/Md + Md/Md)
// 			= R / (1+R)					R = H2O mixing ratio					dimensionless
//
// 2011.11.08 	written
Function SpecificHumidity( R_ )
	variable R_ 		 			// H2O mixing ratio			dimensionless
	return R_ / (1 + R_) 
End


// returns an approximation of specific humidity; exactness can be achieved using SpecificHumdity(MixingRatioVP(...))
//
// 	Arya, S Pal. Introduction to Micrometeorology. 2nd Ed. 2001. Academic Press. ISBN 0-12-059354-8
//		Eqn 5.11	Q ~= (Mw/Md)*(e/P)	Mw,Md = mass of water vapor,dry air 		same units
//										e,P = H2O vapor & barometric pressures	same units
//
// 2011.11.04 	written
Function SpecificHumidityVP( e_, P_ )
	variable e_					// H2O vapor pressure			mbar
	variable P_					// barometric pressure			mbar
	return (kMW_h2o/kMW_dryair)*e_ / P_
End


// returns numeric value of timestamp represented in <timestring>
// use TimeRegEx() to get the correct format; double quotes are stripped prior to parsing
//
// 2011.08		converted from interactive function to utility function
// 2010.old		initial release
Function string2secs( timestring, format ) // not threadsafe, string
	string timestring, format
	variable yy, mm, dd, h, m, s
	sscanf ReplaceString("\"", timestring, ""), format, yy, mm, dd, h, m, s
	return date2secs(yy,mm,dd) + h*3600 + m*60 + s
End


// returns maskStr after replacing appropriate field codes with values derived from inVal
// 	fixed width codes		value derived from inVal	
//	----------------			----------------------------------
//	\nn					inVal as integer		*must specify <fixNNwidth> or variable width is used
//	\YYYY				four-digit year
//	\YY					two-digit year	
//	\MM				month
//	\DD					day of month 
//	{ \DDD				day of year } 		*not implemented*
//	\hh 					hour	 in military style
//	\hhn					hour in normal style
//	\mm				minute
//	\ss					second
//	
//	variable width codes	value derived from inVal
//	-----------------------------	----------------------------------
//	\n					inVal as integer
//	\M					month 
//	\D					day of month 
//	\ddd					day of year 		*not implemented*
//	\h					hour in military style
//	\hn					hour	 in normal style
//	\m					minute
//	\s					second
//
//
// 2011.08		initial release
Function/S StringFromMaskedVar(maskStr, inVal, [fixNNwidth]) // not threadsafe, string
	variable inVal			// number to derive field code values from
	string maskStr		// string containing field codes to interpret
	variable fixNNwidth	// optional specifier of output field width if using inVal as integer
	string nf, nv, tmp, yyf, mmf, mmv, ddf, ddv, doyf, doyv, hmf, hmv, hnf, hnv, mf, mv, sf, sv
	variable temp
	
	If (ParamIsDefault(fixNNwidth)) 		// if not specified, use variable width instead
		maskStr = ReplaceString("\nn", maskStr, "\n")
	endif
	
	sprintf nf, "%0"+num2str(fixNNwidth)+"d", inVal	// fixed digit num
	sprintf nv, "%d", inVal			// variable num
	tmp = secs2date(inVal, -2)	
	yyf = tmp[0,3]				// fixed 4 digit year
	mmf = tmp[5,6]				// fixed 2 digit month
	sprintf mmv, "%d", str2num(mmf) // variable month
	ddf = tmp[8,9]				// fixed 2 digit day
	sprintf ddv, "%d", str2num(ddf)	// variable day
	// doyf = [find out doy fixed width] ****
	// doyv = [make doy variable width] ****
	tmp = secs2time(inVal, 3)
	mf = tmp[3,4]				// fixed 2 digit min
	sprintf mv, "%d", str2num(mf)	// variable min
	sf = tmp[6,7]					// fixed 2 digit sec
	sprintf sv, "%d", str2num(sf)	// variable sec
	hmf = tmp[0,1]				// fixed 2 digit hour, military
	temp = str2num(hmf)
	sprintf hmv, "%d", temp 		// variable hour, military
	temp = Mod(temp, 12)
	sprintf hnf, "%02d", temp		// fixed 2 digit hour, normal
	sprintf hnv, "%d", temp		// variable hour, normal
	
	maskStr = ReplaceString("\nn", maskStr, nf, 1)
	maskStr = ReplaceString("\n", maskStr, nv, 1)
	maskStr = ReplaceString("\YYYY", maskStr, yyf, 1)
	maskStr = ReplaceString("\YY", maskStr, yyf[2,3], 1)
	maskStr = ReplaceString("\MM", maskStr, mmf, 1)
	maskStr = ReplaceString("\M", maskStr, mmv, 1)
	//maskStr = ReplaceString("\DDD", maskStr, doyf, 1)
	//maskStr = ReplaceString("\ddd", maskStr, doyv, 1)
	maskStr = ReplaceString("\DD", maskStr, ddf, 1)
	maskStr = ReplaceString("\D", maskStr, ddv, 1)
	maskStr = ReplaceString("\hhn", maskStr, hnf, 1)
	maskStr = ReplaceString("\hn", maskStr, hnv, 1)
	maskStr = ReplaceString("\hh", maskStr, hmf, 1)
	maskStr = ReplaceString("\h", maskStr, hmv, 1)
	maskStr = ReplaceString("\mm", maskStr, mf, 1)
	maskStr = ReplaceString("\m", maskStr, mv, 1)
	maskStr = ReplaceString("\ss", maskStr, sf, 1)
	maskStr = ReplaceString("\s", maskStr, sv, 1)
	
	//print "[FIXED] #["+nf+"] yr["+yyf+"] mo["+mmf+"] day["+ddf+"] milhr["+hnf+"] normhr["+hmf+"] min["+mf+"] sec["+sf+"]"
	//print "[VARY] #["+nv+"] mo["+mmv+"] day["+ddv+"] milhr["+hnv+"] normhr["+hmv+"] min["+mv+"] sec["+sv+"]"
	return maskStr
End


// Returns truth or nontruth (1 or 0) of whether bit # <bit> is set in <var>
//
// 2011.09		initial release
ThreadSafe Function TestBit( var, bit )
	variable var, bit
	return ( (trunc(var) & (2^bit)) != 0 )
End


// returns a regular expression which matches time format:
//	0	YYYY-MM-DD hh:mm:ss.ss		CampbellSci LoggerNet
// 	1	... expand as needed ...	
//
// 2010.old		initial release
Function/S TimeRegEx( choice ) // not threadsafe, string
	variable choice
	switch ( choice )
		case 5:
		case 4:
		case 3:
		case 2:
		case 1:
			print "these cases are reserved but don't do anything yet"
			return ""
		case 0:
		default:
			//return "%*[\"]%4u%*[-]%2u%*[-]%2u%*[ ]%2u%*[:]%2u%*[:]%5f%*[\"]"
			return "%4u%*[-]%2u%*[-]%2u%*[ ]%2u%*[:]%2u%*[:]%5f"
	endswitch
End


// TODO
// 	potentially rename this function to avoid collisions with variable names
//
// returns turbulent kinetic energy derived from orthogonal wind components
//
//	Air Pollution Meteorology And Dispersion. S. Pal Arya. 1999. Oxford University Press
// 	https://secure.wikimedia.org/wikipedia/en/wiki/Turbulence_kinetic_energy
//		k = (1/2)*[ < ( u' )^2 > + < ( v' )^2 > + < ( w' )^2 > ]
//			<...> denotes averaging		primes denote turbulent quantities
//
// 2011.11.23 	written
Function TKE( u_, v_, w_ [, p1, p2] )
	wave u_, v_, w_ 				// orthogonal wind components 			m / s
	variable p1, p2 				// optional inclusive point boundaries
	
	If ( !SameNumRows(u_, v_) || !SameNumRows(v_, w_) )
		print "TKE: source waves had different number of rows - aborting"
		return NAN
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? DimSize(u_,0) : p2), p1, DimSize(u_,0) )
	
	return ( Cov(u_,u_,p1=p1,p2=p2) + Cov(v_,v_,p1=p1,p2=p2) + Cov(w_,w_,p1=p1,p2=p2) ) / 2
End


// upzips archive <srcFileStr> to directory <destFolderStr> and returns 
// semicolon separated list of unzipped files. 
//
// 2011.09		initial release
Function/S UnzipArchive( srcFileStr, destFolderStr, overwrite, flatten )
	string srcFileStr			// fully qualified path to source archive
		// 	*must be a subdirectory of the 'home' path (ie current experiment location)*
	string destFolderStr 		// "" for temp or unquoted fully qualified path to output directory
	variable overwrite			// 0/unrecognized flag: prompt user
							// 1: overwrite without prompt
							// 2: skip existing files
							// 3: rename extracted files with numeric suffix
							// 4: rename existing files with numeric suffix
	variable flatten			// nonzero to flatten archives upon unzipping

	string prefpath, srcCmd, owCmd, okExtList, appPath_N, destFileList, exestr
	string srcFile, srcExt, srcPath, srcRelPath_N, destPathName, destFolderStr_N
	
	srcFile = ParseFilePath(0, srcFileStr, ":", 1, 0)	// file name + ext
	srcExt = ParseFilePath(4, srcFileStr, ":", 0, 0) 	// file ext
	srcPath = ParseFilePath(1, srcFileStr, ":", 1, 0)	// path only
			
	okExtList = "zip;7z;rar;"						// approved archive extensions (sans .)
	If ( !(1+WhichListItem(srcExt, okExtList)) )
		print "UnzipArchive: not a valid file extension <"+srcFile+"> - aborting."
		return ""
	endif

	prefpath = "root:Packages:ART"				// find app dir in prefs
	DFREF prefs = NewDataFolderX(prefpath)
	appPath_N = StrVarOrDefault(prefpath+":g7zipPath", "C:\\\"Program Files\"\\7-Zip\\7z")
	string/G prefs:g7zipPath = appPath_N
	
	switch (overwrite)								// choose overwrite flag
		case 4: 
			owCmd = " -aot "
			break
		case 3:
			owCmd = " -aou "
			break
		case 2: 
			owCmd = " -aos "
			break
		case 1:
			owCmd = " -aoa "
			break
		case 0:
		default:
			owCmd = ""
	endswitch
	srcCmd = SelectString(flatten, " x ", " e ") 		// 0=x=full path, 1=e=flatten
	
	PathInfo home 
	If ( !stringmatch(srcPath, S_path+"*") )		// reject dirs outside home (experiment location)
		print "UnzipArchive: source directory not within home. sorry this is too hard yet"
		return ""
	endif
	// remove home dir -> relative; make native path; add file name
	srcRelPath_N = ParseFilePath(5, ReplaceString(S_path, srcPath, ""), "\\", 0, 0)+srcFile
	
	If ( !strlen(destFolderStr) )
		destFolderStr = ParseFilePath(5,SpecialDirPath("Temporary", 0, 1, 1),":",0,0)+srcFile+":"
	endif
	destFolderStr_N = ParseFilePath(5, destFolderStr, "\\", 0, 0)
	
	exestr = "cmd /C "+appPath_N+srcCmd+srcRelPath_N+owCmd+" -o\""+destFolderStr_N+"\""
	ExecuteScriptText exestr
	
	destPathName = UniqueName("temp", 12, 0)
	NewPath/Z/Q $destPathName, destFolderStr_N
	If (V_flag)
		print "UnzipArchive: failed to locate destination directory <"+destFolderStr_N+"> - aborting."
		return ""
	endif
	destFileList = ListFilesIn(destPathName, "", "????", -1, -1)
	KillPath $destPathName
	return destFileList
End


// unpacks any archive found in <fileList> to temp location and replaces archive's name
// in <fileList> with list of files inside archive 
//
// 2011.09		initial release
Function/S UnzipArchivesInList( fileList ) 
	string fileList			// semicolon separated list of full file paths
	
	string fileName, unzipList
	variable i
	for (i=0; i<ItemsInList(fileList); i+=1)
		fileName = StringFromList(i, fileList)
		If ( 1+WhichListItem(ParseFilePath(4, fileName, ":", 0, 0), "zip;7z;rar;") )
			unzipList = UnzipArchive( fileName, "", 1, 1 )
			fileList = RemoveListItem(i, fileList)
			fileList = AddListItem(unzipList, fileList, ";", i)
			fileList = RemoveFromList("", fileList) 	// remove blank entries
			i -= 1 	// back-track to check 1st file in archive on next loop
		endif
	endfor
	return fileList
End


// updates the named progress window
//
// 2011.11.07 	made <msg> optional param
// 2011.10.28		initial working version
Function UpdateProgressWindow( name, val1, val2, [msg, noKill] )
	string name 			// name of progress window to affect
	variable val1, val2		// status values
		// 	IF val1 < 0, THEN progress is assumed to be indefinite
		// 	IF val2 <= 0, THEN val1 is interpreted as a ratio (0-1) representing % complete
		// 	OTHERWISE the value ( val1 / val2 ) as a ratio (0-1) is used to represent % complete
	string msg			// status message displayed in frame
	variable noKill		// optional: nonzero to prevent progress window from closing at 100%
	
	DoWindow $name
	If ( !V_flag )
		return -1
	endif
	
	variable dispval, dispmode
	string progstr
	If ( val1 < 0 )
		dispval = 1
		dispmode = 4
		progstr = "Status: indefinite"
	elseif ( val2 <= 0 )
		dispval = Limit(val1, 0, 1)
		dispmode = 3
		sprintf progstr, "%.0f%% complete", dispval*100
	else
		dispval = Limit( val1/val2, 0, 1)
		dispmode = 3
		sprintf progstr, "%d/%d complete", val1, val2
	endif
	msg = SelectString( strlen(msg), " ", msg)	// replaces empty strings with single space
	
	ValDisplay vdProgress,win=$name,mode=dispmode,value=_NUM:dispval
	TitleBox titleProgress,win=$name,title=progstr
	TitleBox titleMsg,win=$name,title=msg
	DoUpdate/W=$name/E=1
	If ( V_flag == 2 )
		KillWindow $name
		Abort
	endif
	
	If ( (dispval >= 1) && (dispmode == 3) && !noKill )	// no killing for indefinite mode
		variable t0 = ticks
		do
		while ( ticks < (t0+30) )	// show 100% for half-second before closing window
		KillWindow $name
	endif
	return 0
End


// Rotates high frequency orthogonal wind components Ux, Uy, and Uz into a stream-wise coordinate system 
// denoted U, V, W where U is along the mean wind direction, V is the cross-wind and W is the vertical axis. 
//
// Input wave IS modified. Returns reference to 2D wave with 1st,2nd,3rd rotation angles in columns 0,1,2,
// labeled yaw, pitch, roll, respectively.
// 	For errors, returns NAN, including wrong size matrix, unrecognized <type> 
//
// 2011.11.03 	renamed angles according to their rotation axis (yaw, pitch, roll); added units (degrees) to 
//				returned wave ref; now redimensioning waves back to 1D after copying from matrices
// 2011.08		initial release
Function/WAVE uvwRotation( uvwMatrix, type )
	wave uvwMatrix		// 2D wave where 	column 1: Ux		
						// 				column 2: Uy	
						//				column 3: Uz
	variable type			// rotation type		0	double rotation
						//					1	triple rotation
						//	NOTE: the planar fit method is performed using several averaging periods
						//	If you wish to do a planar fit, see IntervaluvwRotation(...)
	If (DimSize(uvwMatrix, 1) != 3) // if not 3 column
		return NAN
	endif
	variable i, mu, mv, mw, yaw, pitch, roll 	

	switch(type) 											// depending on type of rotation
		case 0:												// if double rotation
		case 1: 												// or triple rotation
			// Springer Handbook of Experimental Fluid Mechanics, Volume 1 (pp 440-443)
			// By Cameron Tropea, Alexander L. Yarin, John F. Foss
			//
			// Statistical Methods in the Atmospheric Sciences (Section 9.3)
			// By Daniel S. Wilks
			// 																			  [ Ux ]
			// Let <> denote mean value; define a wind vector in coordinate system x,y,z as [Ro] = [ Uy ]
			//																			  [ Uz ]
			// 1st rotation aligns U axis along mean WD by rotating around Z-axis by angle yaw (y)
			// 	y = atan2(<Uy>, <Ux>)
			//
			//		   [ Ux* ] 			[   cos(y) 	sin(y) 	0 ]	[ Ux ] 	[ (Ux)cos(y) + (Uy)sin(y) ]
			// 	[R*] = [ Uy* ] = [A][Ro] = 	[  -sin(y)		cos(y) 	1 ] 	[ Uy ] = 	[ (Uy)cos(y) - (Ux)sin(y)  ]
			//		   [ Uz* ]				[      0 		     0 	1 ] 	[ Uz ] 	[ Uz 				    ]
			// 
			// Extending the wind vector Ro to represent a time series results in the following matrix math:
			//
			// 		   [	X1 	X2 	X3 	X4 	X5 	X6 	X7 	...	Xn 	]
			// 	[Ro] = [ 	Y1 	Y2 	Y3 	Y4 	Y5 	Y6 	Y7 	... 	Yn 	] 	with [A] defined as above & 
			// 		   [ 	Z1 	Z2 	Z3 	Z4 	Z5 	Z6 	Z7	...	Zn 	] 		CY/SY= cos(y)/sin(y)
			// 
			// 		   [ CB 	SY 	0 ] [ X1 	... 	Xn ]  	[ X1(CY)+Y1(SY) 		... 	Xn(CY)+Yn(SY) ]
			// 	[R*] = [ -SY 	CY 	0 ] [ Y1	...	Yn ] = 	[ X1(-SY)+Y1(CY)		... 	Xn(-SY)+Yn(CY)]
			// 		   [  0	  0	1 ] [ Z1 	... 	Zn]    	[ Z1 	   			... 	Zn 			    ] 
			//
			Duplicate/FREE/R=[][0] uvwMatrix, unf
			Duplicate/FREE/R=[][1] uvwMatrix, vnf
			Redimension/N=(-1,0) unf, vnf
			Make/WAVE/FREE/N=2 nanList = {unf, vnf}
			RemoveNansW( nanList ) 	
			mu = mean(unf)
			mv = mean(vnf)
			yaw = atan2(mv, mu) 								// first rotation angle
			Make/FREE/N=(3,3) rot1 = { {cos(yaw), -1*sin(yaw), 0}, {sin(yaw), cos(yaw), 0}, {0, 0, 1} } // [A]
			MatrixOp/FREE uvw1 = ( rot1 x uvwMatrix^t )^t 			// multiply matrices & transpose results
			WAVEclear unf, vnf, nanList, rot1 						// free up memory
			//
			// 2nd rot. corrects vertical tilt in plane of mean WD by rotating around new y axis (y*) by angle pitch (p)
			// 	p = atan2(<Uz*>, <Ux*>)
			//
			// 		   [ Ux^ ]				[   cos(p)    0 	sin(p) ] [ Ux* ] 	[ (Ux*)cos(p) + (Uz*)sin(p) ]
			// 	[R^] = [ Uy^ ] = [B][R*] = 	[    0 	   1 		   0   ] [ Uy* ] = 	[ Uy 				     ]
			//		   [ Uz^ ] 			[  -sin(p)	   0 		cos(p)] [ Uz* ] 	[ (Uz*)cos(p) - (Ux*)sin(p)  ]
			//
			// 		   [	X1 	X2 	X3 	X4 	X5 	X6 	X7 	...	Xn 	]
			// 	[R*] = [ 	Y1 	Y2 	Y3 	Y4 	Y5 	Y6 	Y7 	... 	Yn 	] 	with [B] defined as above & 
			// 		   [ 	Z1 	Z2 	Z3 	Z4 	Z5 	Z6 	Z7	...	Zn 	] 		CP/SP = cos(p)/sin(p)
			// 
			// 		   [ CP 	 0 	SP ] [ X1 	... 	Xn ]  	[ X1(CP)+Z1(SP) 		... 	Xn(CP)+Zn(SP) ]
			// 	[R^] = [  0	 1 	 0  ] [ Y1		...	Yn ]  = 	[ Y1 				... 	Yn 			   ]
			// 		   [ -SP	 0	CP ] [ Z1 	... 	Zn ]    	[ X1(-SP)+Z1(CP)	   	... 	Xn(-SP)+Zn(CP)] 
			// 
			Duplicate/FREE/R=[][0] uvw1, unf 
			Duplicate/FREE/R=[][2] uvw1, wnf
			Redimension/N=(-1,0) unf, wnf
			Make/WAVE/FREE/N=2 nanList = {unf, wnf} 
			RemoveNansW( nanList ) 
			mu = mean(unf) 
			mw = mean(wnf)
			pitch = atan2(mw, mu) 								// second rotation angle
			Make/FREE/N=(3,3) rot2 = { {cos(pitch), 0, -1*sin(pitch)}, {0, 1, 0}, {sin(pitch), 0, cos(pitch)} } // [B]
			MatrixOp/FREE uvw2 = ( rot2 x uvw1^t )^t				// perform 2nd rotation on first results
			WAVEclear uvw1, unf, wnf, nanList 						// remove temps from memory
			If ( type )												// if doing triple rotation
				//
				// 3rd rotation corrects vertical tilt in plane of mean cross wind by rotating so final Z-axis is normal
				// to mean wind trajectory. If done by suggesting the approximation Cov( v, w ) = 0 [in the final 
				// coordinate system], then rotation occurs around the new x axis (x^) by an angle roll (r)
				//
				// 	r = (1/2)*atan2( 2*Cov(Uy^, Uz^) / (Var(Uy^) - Var(Uz^)) )
				// 
				// 		   [ u ]			 [    1 	      0 	    0    ] [ Ux^ ] 	[ Ux^ 					]
				// 	[R]  = [ v  ] = [C][R^] = [    0 	  cos(r) 	sin(r)  ] [ Uy^ ] = 	[ (Uy^)cos(r) + (Uz^)sin(r) ]
				// 		   [ w ] 			 [    0 	 -sin(r)  	cos(r) ] [ Uz^ ] 	[ (Uz^)cos(r) - (Uy^)sin(r)  ]
				//
				// 		   [	X1 	X2 	X3 	X4 	X5 	X6 	X7 	...	Xn 	]
				// 	[R^] = [ 	Y1 	Y2 	Y3 	Y4 	Y5 	Y6 	Y7 	... 	Yn 	] 	with [C] defined as above &
				// 		   [ 	Z1 	Z2 	Z3 	Z4 	Z5 	Z6 	Z7	...	Zn 	] 		 CR/SR = cos(r)/sin(r)
				//  
				// 		   [  1 	 0	 0  ] [ X1 	... 	Xn ]    	[ X1  				...	Xn 				]
				// 	[R] =  [  0 	CT	ST] [ Y1		...	Yn ] = 	[ Y1(CR)+Z1(SR)  	...	Yn(CR)+Zn(SR) 	]
				// 		   [  0 	-ST  ST] [ Z1 	... 	Zn ]  	[ Y1(-SR)+Z1(CR) 	...	Yn(-SR)+Zn(CR)  ]	
				//
				// Significant disagreement exists regarding a 3rd rotation - it should be done only with purpose!
				//
				Duplicate/FREE/R=[][1] uvw2, vnf
				Duplicate/FREE/R=[][2] uvw2, wnf
				Redimension/N=(-1,0) vnf, wnf
				Make/WAVE/FREE/N=3 nanList = {vnf, wnf} 	
				RemoveNansW( nanList )
				roll = (0.5)*atan(( 2*Cov(vnf, wnf) )/( Variance(vnf) - Variance(wnf) )) // third rotation angle									// clear values to avoid relics
				Make/FREE/N=(3,3) rot3 = { {1, 0, 0}, {0, cos(roll), -1*sin(roll)}, {0, sin(roll), cos(roll)} } // [C]
				MatrixOp/FREE uvw3 = ( rot3 x uvw2^t )^t 				// perform third rotation
				WAVEclear uvw2, vnf, wnf, nanList 						// clear up memory
				
				uvwMatrix[][] = uvw3[p][q]		// set input wave = rotation 3 wave
			else
				uvwMatrix[][] = uvw2[p][q]		// set input wave = rotation 2 wave
			endif
			break
		default:
			return NAN
			break
	endswitch
	
	Make/FREE/N=(1,3) rotAngles = {{yaw}, {pitch}, {roll}}		// angles wave 
	rotAngles = R2D(rotAngles)
	SetDimLabel 1, 0, yaw, rotAngles
	SetDimLabel 1, 1, pitch, rotAngles
	SetDimLabel 1, 2, roll, rotAngles
	SetScale d, 0, 0, "degrees", rotAngles
	return rotAngles
End


// returns virtual temp == temp. at which a moist air parcel would occupy the same volume/pressure as a dry air volume
//
// 	http://amsglossary.allenpress.com/glossary/search?id=virtual-temperature1
//				Tv = T*(1 + R/µ) / (1 + R)			T = ambient absolute temperature	Kelvin
//												R = water vapor mixing ratio		dimensionless
//				Tv ~= T*(1 + 0.61*R) 				µ = Rd/Rv = MWv/MWd ~= 0.622	dimensionless
// 	
// 	Arya, S Pal. Introduction to Micrometeorology. 2nd Ed. 2001. Academic Press. ISBN 0-12-059354-8
// 		5.15 	Tv = T[ 1 + (MWd/MWv -1)*Q ] 		T = ambient absolute temperature	Kelvin
//												Q = specific humidity ~= R 		dimensionless
//												R = mixing ratio					dimensionless
//		5.15 	Tv ~= T*(1 + 0.61*Q) 				MWd/MWv = Rv/Rd = 1/ 0.622		dimensionless
//
// 	Derived from Eqn 2.17 (Wallace and Hobbs) and Eqn 5.11 (Arya) approx. for specific humidity:
// 			Tv 	= T / [ 1 - (e/P)(1 - µ) ]				µ ~= 0.622
//				= T / [ 1 - (0.378)*(e/P) ]			Q ~= 0.622*e/P >> (Q/0.622) ~= (e/P)
// 				= T / [ 1 - 0.61*Q ]	<<< eerily similar to Eqn 2.62
//
// 2011.11.11 	adapted to Celcius
// 2011.11.04		written
Function VirtualTemp( T_, R_ )
	variable T_			// ambient abs. temp.			Celcius
	variable R_ 			// H2O mixing ratio			ratio, 0-1
	return (T_+273.15)*(1+0.61*R_)-273.15 	// 			Celcius
End


// returns virtual temp == temp. at which a moist air parcel would occupy the same volume/pressure as a dry air volume
//
// 	Atmospheric Science An Introductory Survey by John Wallace and Peter Hobbs. Academic Press, 1977. ISBN 0-12-732950-1
//		2.17 	Tv = T / [ 1 - (e/P)(1 - µ) ]			T = ambient absolute temperature	Kelvin
//												e = water vapor pressure			mbar
//		2.62		Tv ~= T*(1 + 0.61*R) 				P = barometric pressure			mbar
//												R = water vapor mixing ratio		dimensionless
// 												µ = Rd/Rv = MWv/MWd ~= 0.622	dimensionless
//
// 2011.11.14		verified results from each version agree well (they do agree to within ppt)
// 2011.11.11 	written
Function VirtualTempVP( T_, e_, P_ )
	variable T_ 			// ambient temp 				Celcius
	variable e_			// H2O vapor pressure			mbar
	variable P_			// barometric pressure 		mbar
	return (T_+273.15)/(1-0.378*(e_/P_))-273.15 	// 		Celcius
end


// returns wave of references to waves listed in <wlist> 
// if <makeFreeCopies> is nonzero, references will be to free duplicates, not originals
// 
// 2011.10.27 	renamed from WaveRefsFromList to WaveList2Refs, created opposite function too
// 2011.09		intial release
Function/WAVE WaveList2Refs( wlist, makeFreeCopies )
	string wlist						// semicolon separated list of waves
	variable makeFreeCopies			// nonzero to make free duplicates (preserve original data)
	variable i, n = ItemsInList(wlist)
	Make/WAVE/FREE/N=(n) wrefs
	for (i=0; i<n; i+=1)
		wave this = $StringFromList(i, wlist)
		If (makeFreeCopies)
			Duplicate/FREE this, that
			wrefs[i] = that
		else
			wrefs[i] = this
		endif
	endfor
	return wrefs
end

// returns string list of waves in <wrefs>; wave names are possibly quoted
// if <fullName> is nonzero, string list contains fully-qualified wave names
// 
// 2011.10.27 	initial release
Function/S WaveRefs2List( wrefs, fullName )
	wave/WAVE wrefs 		// wave of references to convert to list
	variable fullName			// nonzero to use fully-qualified wave names (ie, folder+name)
	string wlist = ""
	variable i
	for (i=0; i<numpnts(wrefs); i+=1)
		wave/Z this = wrefs[i]
		If ( !WaveExists(this) )
			continue
		elseif ( fullName )
			wlist += GetWavesDataFolder(this, 2)
		else
			wlist += PossiblyQuoteName(NameOfWave(this))
		endif
	endfor
	return wlist
end



// TODO
//	determine whether a sonic type flag is even necessary
//
// Returns wind direction calculated from pre-averaged horizontal wind components
// 	WD = atan( -Ux / -Uy )*(deg/rad) + azimuth + Vaz
//
// 2011.09 		added sonic type code in case, marked threadsafe
// 2010.old		initial release
ThreadSafe Function WindDir( Ux, Uy, azimuth, type )
	variable Ux, Uy 		// sonic anemometer U & V component
	variable azimuth 		// sonic azimuth to north; add a declination if needed
	variable type 			// sonic type code
	
	switch(type)
		case 0:		// CSAT3 & ATI style
			return ModWD( R2D(atan2( -1*Ux, -1*Uy)) + azimuth + 90 )
		default:
			return NAN
	endswitch
End

// TODO
//	determine if this function requires a sonic type code to pass to WindDir()
//
// Returns estimated std. dev. of wind direction by Mardia formula or NAN for failure
//
// Eq 2.6.8, EPA Metereological Monitoring Guidance for Regulatory Modeling Applications (2000)
// also: http://www.webmet.com/met_monitoring/621.html
//
// Since the Mardia formula depends on the magnitude of mean sin() and mean cos() of WD, it is
// not necessary to rotate the WD's into a meterological coordinate system prior to calculating. 
//
// 2011.10.12 	revised WD assignment to prevent index over-range
// 2011.10.11		added Limit()s for point boundaries
// 2011.10.07		introduced optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindDirMardiaSdev( Ux, Uy, [p1, p2] )
	wave Ux, Uy			// sonic anemometer U, V components
	variable p1, p2		// optional point boundaries to operate within (inclusive), default to full range
	
	if ( !SameNumPnts(Ux, Uy) )				// if not same length
		return NAN 								// quit, return no num
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	// Mardia estimate of WD std dev, based on directional statistics
	// Eq 2.6.8, EPA Metereological Monitoring Guidance for Regulatory Modeling Applications (2000)
	// 	Sa 		= mean( sin( WD[i] ) )
	//	Ca 		= mean( cos( WD[i] ) )
	//	R 		= sqrt( Sa^2 + Ca^2 )
	// 	stdev 	= sqrt( -2*ln(R) ) 			[in radians]
	Duplicate/FREE/R=[p1,p2] Ux, WD, sinw, cosw
	WD = NAN; sinw = NAN; cosw = NAN
	WD[0, p2-p1] = WindDir( Ux[p1+p], Uy[p1+p], 0, 0)
	sinw = sin( WD[p]*(PI/180) ) 
	cosw = cos( WD[p]*(PI/180) ) 
	variable R = sqrt( mean(sinw)^2 + mean(cosw)^2 )
	return R2D( sqrt( -2*ln(R) ) ) 					// finish calc & return
End


// TODO
// 	evaluate impact of method adaptation
// 	verify the standard deviation values
//
// Returns complex variable or NAN if not same length, has NANs
// 		real part 		scalar mean wind direction				real(...)
// 		imag part	std. dev of scalar wind direction 		imag(...)
// Calculations performed following a modified Mitsuta method for single-pass wind direction
// Refer to Eq. 6.2.4, EPA Metereological Monitoring Guidance for Regulatory Modeling Applications (2000)
//
// 2011.10.12		revised WD assignment to prevent index over-range
// 2011.10.11		added Limit()s for point boundaries
// 2011.10.7		introduced optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
Function/C WindDirScalarMeanSdev( Ux, Uy, azimuth, flag, [p1, p2] )
	wave Ux, Uy				// horizontal wind components U, V
	variable azimuth 			// sonic azimuth from North; include declin here if needed
	variable flag 				// reserved for sonic type code
	variable p1, p2			// optional point boundaries (inclusive) default to full range
	
	variable R, phi, i, L, val					// working vars

	If ( !SameNumPnts(Ux, Uy) ) 				// if diff length
		return NAN 								// return
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	// Mitsuta single-pass method for scalar mean wind direction
	// Eq. 6.2.4, EPA Metereological Monitoring Guidance for Regulatory Modeling Applications (2000)
	//	WD[i] 	WD for i-th sample in range ={ 0 <= WD[i] < 360 }
	//	D[i] 		instant scalar WD 	= WD[i]					i = 1
	// 	D[i] 							= D[i-1] + phi + 360 		i > 1 & phi < -180
	//	D[i] 							= D[i-1] + phi 			i > 1 & abs(phi) < 180
	//	D[i] 							= D[i-1] + phi - 360		i > 1 & phi > 180
	//	D[i] 							= undefined 				i > 1 & phi = 180
	//	phi 		delta(inst WD) 		= WD[i] - D[i-1]			i > 1
	// 	WD 	mean scalar WD 		= mean(D) 
	Duplicate/FREE/R=[p1,p2] Ux, WD, D
	WD = NAN; D = NAN
	WD[0, p2-p1] = WindDir( Ux[p1+p], Uy[p1+p], azimuth, flag) 	// populate instant WD wave in range 0-360

	for (i=0; i<numpnts(WD); i+=1)				// for each sampled point
		if (numtype(WD[i]))								// if invalid #
			D[i] = NAN										// undefined
		elseif ( !i || numtype(D[i-1]) ) 						// if on first row or after any gap
			D[i] = WD[i]										// set D = WD
			val = val ? val : D[i] 								// set val = D[i] only if not set (ie still val=0)
			if ( i && D[i]-R < -180 )								// except 1st row, if diff > 180* along neg pole
				D[i] += 360										// swap to pos pole
			elseif ( i && D[i]-R > 180 )							// except 1st row, if diff > 180* along pos pole
				D[i] -= 360 										// swap to neg pole
			endif
			// no adjustments made if D[i] is within 180* of last value
		else 											// no errors and i > 1
			phi = WD[i] - D[i-1]								// calc delta WD
			if ( phi < -180 ) 									// if diff is > halfway past neg pole
				D[i] = D[i-1] + phi + 360 							// adjust to < halfway of pos pole
			elseif ( phi > 180 ) 								// if diff is > halfway past pos pole
				D[i] = D[i-1] + phi - 360 							// adjust to < halfway of neg pole
			elseif ( abs(phi) < 180 ) 							// if diff is < halfway to either pole
				D[i] = D[i-1] + phi 								// make no adjustments
			elseif ( phi == 180 )								// if precisely 180* difference
				D[i] = NAN 										// defined as undefined
			endif	
															// *** method modification ***
			if ( D[i] - val > 180 ) 								// if meanders above 180* of initial WD 
				D[i] -= 360 										// bring within
			elseif ( D[i] - val < -180 ) 							// if meanders below 180* of initial WD
				D[i] += 360 										// bring within
			endif

			R = (numtype(D[i])) ? R : D[i] 						// update last value unless D[i] is invalid #					
		endif
	endfor
	variable foo = mean(D)
	variable bar = variance(D)
	DoUpdate
	return cmplx( ModWD(mean(D)), sqrt(variance(D)) ) 		// return both vals in complex num
End


// Returns direction of unit wind vector or NAN if any points were NAN or diff wave lengths
//
// Psuedo-formula: WD = atan( -1*mean( <Ux> ) / -1*mean( <Uy> ) )*(deg/rad) + azimuth + declin + Vaz
//	where 	<Ux[i]> = Ux[i] / sqrt( Ux[i]^2 + Uy[i]^2 )
// 	and 		<Uy[i]> = Uy[i] / sqrt( Uy[i]^2 + Uy[i]^2 )
//
// 2011.10.12		revised wave assignments to prevent index over-range
// 2011.10.11		added Limit()s to point boundaries
// 2011.10.07		introduced optional point boundaries, removed duplicate call to WindSpeed()
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindDirUnitVectorMean( Ux, Uy, azimuth, flag, [p1, p2] )
	wave Ux, Uy 				// horizontal wind components U, V
	variable azimuth			// sonic azimuth+declination
	variable flag				// type of sonic 
	variable p1, p2			// optional point boundaries, default to full range
	
	If ( !SameNumPnts( Ux, Uy ) ) 						// if not same length
		return NAN  										// return no num
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	Duplicate/FREE/R=[p1,p2] Ux, unitx, unity, WS
	unitx = NAN; unity = NAN; WS = NAN;
	WS[0, p2-p1] = WindSpeed( Ux[p1+p], Uy[p1+p] )
	unitx[0, p2-p1] = Ux[p1+p] / WS[p]
	unity[0, p2-p1] = Uy[p1+p] / WS[p]
	return WindDir(mean(unitx), mean(unity), azimuth, flag) 
End


// Returns direction of resultant wind vector or NAN if any points were NAN or waves diff length
//
// Psuedo-formula: WD = atan( -1*mean(Ux) / -1*mean(Uy) )*(deg/rad) + azimuth + declin + Vaz
//
// 2011.10.11		added revised Limit()s to point boundaries
// 2011.10.07		introduced optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindDirVectorMean( Ux, Uy, azimuth, type, [p1, p2] )
	wave Ux, Uy 			// horizontal wind components U, V
	variable azimuth 		// sonic azimuth+declination
	variable type 			// reserved for sonic type code
	variable p1, p2		// optional point boundaries (inclusive) default to whole range
	
	If ( !SameNumPnts( Ux, Uy ) ) 						// if not same length
		return NAN  										// return no number
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	return WindDir(mean(Ux,pnt2x(Ux,p1),pnt2x(Ux,p2)), mean(Uy,pnt2x(Uy,p1),pnt2x(Uy,p2)), azimuth, type)
End



// TODO
// 	determine if this function needs to have a sonic type flag passed through it
//
// Returns estimated std. dev of wind direction by Yamartino formula or 
// NAN if any points were NAN or waves diff length
//
// Eq 2.6.9, EPA Meteorological Monitoring Guidance for Regulatory Modeling Applications (2000)
//
// 2011.10.12 	revised WD assignment to prevent index over-range
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07		added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindDirYamartinoSdev( Ux, Uy, [p1, p2] )
	wave Ux, Uy				// horizontal wind components U, V
	variable p1, p2			// optional point boundaries (inclusive) default to full range
	
	If ( !SameNumPnts(Ux, Uy) ) 					// if wave different length
		return NAN 									// quit, return no num
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	// Yamartino single-pass method for estimating WD standard deviation 
	// Eq 2.6.9, EPA Metereological Monitoring Guidance for Regulatory Modeling Applications (2000)
	//	WD 	= instant wind directions
	// 	Sa 		= mean( sin( WD[i] ) ) 
	// 	Ca 		= mean( cos( WD[i] ) ) 
	//	d 		= sqrt( 1 - (Sa^2 + Ca^2) )
	// 	stdev 	= arcsin(d)*[1 + 0.1547d^3] 
	Duplicate/FREE/R=[p1,p2] Ux, WD, sinw, cosw
	WD = NAN; sinw = NAN; cosw = NAN;
	WD[0, p2-p1] = WindDir( Ux[p1+p], Uy[p1+p], 0, 0 )
	sinw = sin( D2R(WD[p]) )
	cosw = cos( D2R(WD[p]) )
	variable d = sqrt(1 - (mean(sinw)^2 + mean(cosw)^2)) // intermediate calc
	return R2D( asin(d)*(1 + 0.1547*d^3) )			// finish calc & return
End

// Returns magnitude of wind speed = sqrt( Ux^2 + Uy^2 ) or NAN if any points were NAN
//
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindSpeed( Ux, Uy )
	variable Ux, Uy 		// horizontal wind components, U,  V
	return sqrt( Ux^2 + Uy^2 ) 				// return magnitude
End


// Returns scalar mean wind speed = mean( sqrt( Ux[i]^2 + Uy[i]^2 ) ) or NAN
// if any points were NAN or waves are not same length
//
// 2011.10.12		revised 2nd WS assignment to prevent index over-range
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07 	added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindSpeedScalarMean( Ux, Uy, [p1, p2] )
	wave Ux, Uy  			// horizontal wind components U, V
	variable p1, p2			// optional point boundaries (inclusive) default to full scale
	
	If ( !SameNumPnts(Ux, Uy) ) 			// if waves are different length
		return NAN 							// quit, return no number
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(Ux)-1 : p2), p1, numpnts(Ux)-1 )
	Duplicate/FREE/R=[p1,p2] Ux, WS
	WS = NAN 
	WS[0, p2-p1] = WindSpeed( Ux[p1+p], Uy[p1+p] )
	return mean(WS)	
End


// Returns scalar harmonic mean wind speed = 1/mean( 1/sqrt(Ux[i]^2 + Uy[i]^2) )
// or NAN if any points were NAN or waves are not same length
//
// 2011.10.12		revised 2nd WS assignment to prevent index over-range
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07		added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindSpeedScalarHMean( Ux, Uy, [p1, p2] )
	wave Ux, Uy 			// horizontal wind components U, V
	variable p1, p2		// optional point boundaries (inclusive) default to full range
	
	If ( !SameNumPnts(Ux, Uy) ) 			// if waves are different length
		return NAN 							// quit, return no number
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	Duplicate/FREE/R=[p1,p2] Ux, WS 
	WS = NAN 
	WS[0, p2-p1] = 1/windSpeed( Ux[p1+p], Uy[p1+p] ) 
	return 1/mean(WS)
End


// Returns standard deviation of scalar wind speed = sqrt( variance( sqrt(Ux[i]^2 + Uy[i]^2) ) )
// or NAN if waves are not same length; NANs in waves are ignored
//
// 2011.10.12 	revised 2nd WS assignment to prevent index over-range
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07		added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindSpeedScalarSdev( Ux, Uy, [p1, p2] )
	wave Ux, Uy			// horizontal wind components U, V
	variable p1, p2		// optional point boundaries (inclusive) default to full range
	
	If ( !SameNumPnts(Ux, Uy) ) 			// if waves different length
		return NAN 							// quit, no return #
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	Duplicate/FREE/R=[p1, p2] Ux, WS 	
	WS = NAN 
	WS[0, p2-p1] = windSpeed( Ux[p1+p], Uy[p1+p] )	
	return sqrt( variance(WS) )
End


// Returns resultant mean wind speed = sqrt( mean(Ux)^2 + mean(Uy)^2 )
// or NAN if any points were NAN or waves are not same length
//
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07		added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
ThreadSafe Function WindSpeedVectorMean( Ux, Uy, [p1, p2] )
	wave Ux, Uy			// horizontal wind components U, V
	variable p1, p2		// optional point boundaries (inclusive) default to full range
	
	If ( !SameNumPnts(Ux, Uy) )			// if diff length
		return NAN							// quit, return no num
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(ux)-1 : p2), p1, numpnts(ux)-1 )
	return sqrt( mean(Ux,pnt2x(Ux,p1),pnt2x(Ux,p2))^2 + mean(Uy,pnt2x(Uy,p1),pnt2x(Uy,p2))^2 ) 
End


// Returns persistence of wind speed = (vector WS) / (scalar WS)
// or NAN if any points were NAN or waves not same length
//
// 2012.06.13 	demarked threadsafe for testing
// 2011.10.11		added revised Limit()s for point boundaries
// 2011.10.07		added optional point boundaries
// 2011.09		marked threadsafe
// 2011.old		made into separate utility function
// 2010.old		initial release (in a monolithic function)
Function WindSpeedPersist( Ux, Uy, [p1, p2] )
	wave Ux, Uy		// horizontal wind components U, V
	variable p1, p2	// optional point boundaries (inclusive) default to full range
	
	If ( !SameNumPnts(Ux, Uy) ) 			// if diff length
		return NAN 	 						// quit, return no num
	endif
	p1 = Limit(p1, 0, p1)
	p2 = Limit( (ParamIsDefault(p2) ? numpnts(Ux)-1 : p2), p1, numpnts(Ux)-1 )
	//return ( WindSpeedVectorMean(Ux,Uy,p1=p1,p2=p2) / WindSpeedScalarMean(Ux,Uy,p1=p1,p2=p2) )
	variable ws_vctr = WindSpeedVectorMean(Ux,Uy,p1=p1,p2=p2) 
	variable ws_sclr = WindSpeedScalarMean(Ux,Uy,p1=p1,p2=p2)
	return ws_vctr / ws_sclr
End




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

Menu "Load Waves"
	"\\M0:/1:Load Campbellsci TOA5 (long header)...", /Q, PromptedLoadCSI()
		help = {"Loads data from Campbell Scientific long-header (TOA5) file(s)"}
	"\\M0::Extended load Campbellsci TOA5...", /Q, PromptedLoadCSI(type=1)
		help = {"Loads data from Campbell Scientific long-header (TOA5) file(s) using extended file selection dialog"}
//	"\\M0::Load CampbellSci TOACI1 (short header)...", /Q, PromptedLoadCSI()
//		help = {"Loads data from Campbell Scientific short-header (TOACI1) file(s)"}
//	"\\M0::Load Los Gatos analyzer file...", /Q, LoadLGR_Panel() // the load button hasn't been tied into anything yet; disabled 2013/6/28 
//		help = {"Launch dialog to load Los Gatos Research CRDS analyzer file(s)"}
//	"\\M0::Load Picarro G1103...", /Q, beep
//		help = {"Loads data from Picarro Fast-Ammonia Analyzer file(s)"}
//	"\\M0::Load Picarro G2301...",  /Q, // use LoadPicarro() manually
//		help = {"Loads data from Picarro Fast-Methane Analyzer file(s)"}
//	"\\M0::Load Generic GPS...", /Q, LoadNMEA()
//		help = {"Loads data from NMEA-formatted text file"}
End


Menu "Analysis"
	Submenu "More tools"
		"\\M0::Simple distance from lat/long pair", /Q, print "Distance =", PromptedLatLongDistance()
	End
End


Menu "Notebook"
	"&Unmake help link", /Q, Notebook kwTopWin textRGB=(0,0,0), fStyle=-1
		help = {"Makes it regular text (not underlined, blue)"}
End


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




// 2013-3-13 	pulled from sandbox for 0.2 release; works for TOA5 files
///////////////////////// PromptedLoadCSI \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
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



// 2013-3-13 	pulled from sandbox for 0.2 release
////////////////////// PromptedLatLongDistance \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
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

















































//===========================================================================\\
#EndIf									// DEVELOPMENT FLAG - MUST BE LAST LINE OF FILE  \\
//===========================================================================\\