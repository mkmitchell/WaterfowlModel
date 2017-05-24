# Final output script called by waterfowlmodel.py
# Set the necessary product code
# import arcinfo


# Import arcpy module
import arcpy, os, sys, getopt, datetime

# Required parameters
# Name for the feature classes within the geodatabase
mav = "mav"
wgcp = "wgcp"
naturalflood = "Natural_flood"
publicinput = "Public_output"
stateboundary = "state_boundaries"

arcpy.env.overwriteOutput = True;
# Setup model specifics
def runWaterfowl (region, workspace, gdb):

	gdb = os.path.join(workspace, gdb)
	scratchgdb = os.path.join(workspace, region + "_scratch.gdb")
	if (region == 'mav'):
		aoi = os.path.join(gdb, mav)
	else:
		aoi = os.path.join(gdb, wgcp)
		
	#setup databases
	if not (os.path.exists(scratchgdb)):
                print("Creating scratch GDB")
		arcpy.CreateFileGDB_management(workspace, region + "_scratch.gdb")
	else:
                print("Scratch GDB already exists.  Using it")

        #check for setup files
        if not (arcpy.Exists(aoi)):
                print("Area of interest '" + aoi + "' does not exist")
                sys.exit(2)
        if not (arcpy.Exists(os.path.join(gdb, naturalflood))):
                print("Natural Flood '" + naturalflood + "' does not exist")
                sys.exit(2)
        if not (arcpy.Exists(os.path.join(gdb, publicinput))):
                print("Public input '" + publicinput + "' does not exist")
                sys.exit(2)
        if not (arcpy.Exists(os.path.join(gdb, stateboundary))):
                print("State boundaries '" + stateboundary + "' does not exist")
                sys.exit(2)


        pubinput = os.path.join(gdb, publicinput)
        inmemerasestr = os.path.join(scratchgdb,"erase")
        stateinput = os.path.join(gdb, stateboundary)
        #Doing the work
        # Process: Erase
        print "Erasing public land data from flood"
        inmemerase = arcpy.Erase_analysis(os.path.join(gdb,naturalflood), pubinput, os.path.join(scratchgdb,"erase"), "")
        # Merge
        print "Merging Natural flood (no public lands) with public lands"
        pubflood = arcpy.Merge_management([inmemerase, pubinput], os.path.join(scratchgdb, "pubflood"), 'STATE \"STATE\" true true false 18 Text 0 0 ,First,#,' + pubinput + ' ,STATE,-1,-1,' + inmemerasestr + ' ,STATE,-1,-1;BCR_NAME \"BCR_NAME\" true true false 27 Text 0 0 ,First,#,' + pubinput + ' ,BCR_NAME,-1,-1;ACRES \"ACRES\" true true false 19 Double 3 18 ,First,#,' + pubinput + ' ,ACRES,-1,-1,' + inmemerasestr + ' ,ACRES,-1,-1;HECTARES \"HECTARES\" true true false 19 Double 3 18 ,First,#,' + pubinput + ' ,HECTARES,-1,-1;WATERSHED \"WATERSHED\" true true false 45 Text 0 0 ,First,#,' + pubinput + ' ,WATERSHED,-1,-1,' + inmemerasestr + ' ,WATERSHED,-1,-1;Z_RED_OAK_ \"Z_RED_OAK_\" true true false 19 Double 0 0 ,First,#,' + pubinput + ' ,Z_RED_OAK_,-1,-1,' + inmemerasestr + ' ,Z_RED_OAK_,-1,-1;HABITAT_TY \"HABITAT_TY\" true true false 16 Text 0 0 ,First,#,' + pubinput + ' ,HABITAT_TY,-1,-1,' + inmemerasestr + ' ,HABITAT_TY,-1,-1;Z_HARVESTE \"Z_HARVESTE\" true true false 19 Double 0 0 ,First,#,' + pubinput + ' ,Z_HARVESTE,-1,-1,' + inmemerasestr + ' ,Z_HARVESTE,-1,-1;MANAGING_A \"MANAGING_A\" true true false 254 Text 0 0 ,First,#,' + pubinput + ' ,MANAGING_A,-1,-1,' + inmemerasestr + ' ,MANAGING_A,-1,-1;COMMON_NAM \"COMMON_NAM\" true true false 100 Text 0 0 ,First,#,' + pubinput + ' ,COMMON_NAM,-1,-1,' + inmemerasestr + ' ,COMMON_NAM,-1,-1;MANAGEMENT \"MANAGEMENT\" true true false 100 Text 0 0 ,First,#,' + pubinput + ' ,MANAGEMENT,-1,-1,' + inmemerasestr + ' ,MANAGEMENT,-1,-1;FUNCTIONAL \"FUNCTIONAL\" true true false 1 Text 0 0 ,First,#,' + pubinput + ' ,FUNCTIONAL,-1,-1,' + inmemerasestr + ' ,FUNCTIONAL,-1,-1;OWNER \"OWNER\" true true false 20 Text 0 0 ,First,#,' + pubinput + ' ,OWNER,-1,-1,' + inmemerasestr + ' ,OWNER,-1,-1;COVER_TYPE \"COVER_TYPE\" true true false 254 Text 0 0 ,First,#,' + pubinput + ' ,COVER_TYPE,-1,-1,' + inmemerasestr + ' ,COVER_TYPE,-1,-1;DED \"DED\" true true false 19 Double 0 0 ,First,#,' + pubinput + ' ,DED,-1,-1;MANAGE \"MANAGE\" true true false 5 Short 0 5 ,First,#,' + inmemerasestr + ' ,MANAGE,-1,-1;BASIN__HUC \"BASIN__HUC\" true true false 29 Text 0 0 ,First,#,' + inmemerasestr + ' ,BASIN__HUC,-1,-1;PROTECTION \"PROTECTION\" true true false 20 Text 0 0 ,First,#,' + inmemerasestr + ' ,PROTECTION,-1,-1;SEEDINDEX \"SEEDINDEX\" true true false 19 Double 0 0 ,First,#,' + inmemerasestr + ' ,SEEDINDEX,-1,-1;WTRCNTRL \"WTRCNTRL\" true true false 5 Text 0 0 ,First,#,' + inmemerasestr + ' ,WTRCNTRL,-1,-1;PUMP \"PUMP\" true true false 5 Text 0 0 ,First,#,' + inmemerasestr + ' ,PUMP,-1,-1;REF_HAB \"REF_HAB\" true true false 5 Text 0 0 ,First,#,' + inmemerasestr + ' ,REF_HAB,-1,-1;REFHABAC \"REFHABAC\" true true false 19 Double 0 0 ,First,#,' + inmemerasestr + ' ,REFHABAC,-1,-1;DEDCALC \"DEDCALC\" true true false 13 Float 0 0 ,First,#,' + inmemerasestr + ' ,DEDCALC,-1,-1')
        #pubflood = arcpy.Merge_management([inmemerase, pubinput], os.path.join(scratchgdb, "pubflood"))
        # Feature class to feature class for proper field mapping and cleanup
        print "Cleaning up feature class and correcting fields"
        publicAndFlood = arcpy.FeatureClassToFeatureClass_conversion(pubflood, scratchgdb, "Public_and_Natural", "\"HABITAT_TY\" <> '' AND \"ACRES\" >=1", "BCR_NAME \"BCR_NAME\" true true false 16 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,BCR_NAME,-1,-1;STATE \"STATE\" true true false 2 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,STATE,-1,-1;MANAGING_A \"MANAGING_A\" true true false 35 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,MANAGING_A,-1,-1;MANAGEMENT \"MANAGEMENT\" true true false 30 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,MANAGEMENT,-1,-1;COMMON_NAM \"COMMON_NAM\" true true false 30 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,COMMON_NAM,-1,-1;ACRES \"ACRES\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,ACRES,-1,-1;HECTARES \"HECTARES\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,HECTARES,-1,-1;HABITAT_TY \"HABITAT_TY\" true true false 16 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,HABITAT_TY,-1,-1;COVER_TYPE \"COVER_TYPE\" true true false 16 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,COVER_TYPE,-1,-1;Z_HARVESTE \"Z_HARVESTE\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,Z_HARVESTE,-1,-1;Z_RED_OAK_ \"Z_RED_OAK_\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,Z_RED_OAK_,-1,-1;FUNCTIONAL \"FUNCTIONAL\" true true false 1 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,FUNCTIONAL,-1,-1;DED \"DED\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,DED,-1,-1;WATERSHED \"WATERSHED\" true true false 45 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,WATERSHED,-1,-1;OWNER \"OWNER\" true true false 20 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,OWNER,-1,-1;MANAGE \"MANAGE\" true true false 10 Long 0 10 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,MANAGE,-1,-1;BASIN__HUC \"BASIN__HUC\" true true false 29 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,BASIN__HUC,-1,-1;PROTECTION \"PROTECTION\" true true false 20 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,PROTECTION,-1,-1;SEEDINDEX \"SEEDINDEX\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,SEEDINDEX,-1,-1;WTRCNTRL \"WTRCNTRL\" true true false 5 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,WTRCNTRL,-1,-1;PUMP \"PUMP\" true true false 5 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,PUMP,-1,-1;REF_HAB \"REF_HAB\" true true false 5 Text 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,REF_HAB,-1,-1;REFHABAC \"REFHABAC\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,REFHABAC,-1,-1;DEDCALC \"DEDCALC\" true true false 13 Float 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,DEDCALC,-1,-1;Shape_Leng \"Shape_Leng\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,Shape_Leng,-1,-1;Shape_Area \"Shape_Area\" true true false 19 Double 0 0 ,First,#," + scratchgdb + " \\Public_and_Natural_temp,Shape_Area,-1,-1", "")
        # Clipping state boundary with focus area
        print "Clipping state boundary aoi with flood, and public lands"
        stateclipped = arcpy.Clip_analysis(stateinput, aoi, os.path.join(scratchgdb,"stateclip"))
        pubfloodclipped = arcpy.Clip_analysis(stateclipped, publicAndFlood, os.path.join(scratchgdb,"pubfloodclip"))
        # Union public lands and flooding with state name and aoi
        print "Union all that stuff and clip with the focus area"
        pubfloodstateunion = arcpy.Union_analysis ([pubfloodclipped, publicAndFlood], os.path.join(scratchgdb,"pfsunion"), "ALL")
        pubfloodaoi = arcpy.Clip_analysis(pubfloodstateunion, aoi, "in_memory/pfaoi")
        # Calculate some fields
        print "Calculating some cool fields"
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "STATE", "Calc( !STATE!, !STATE_ABBR!)", "PYTHON_9.3", "def Calc(st, stabbr):\\n  if (st == '' or not st):\\n    return stabbr\\n  else:\\n    return st")
        pubfloodaoi = arcpy.Select_analysis(pubfloodaoi, "in_memory/selectt", "STATE <> ''")
        pubfloodaoi = arcpy.AddField_management(pubfloodaoi, "FIXID", "LONG", "", "", "", "", "NULLABLE", "NON_REQUIRED", "")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "FIXID", "!OBJECTID!", "PYTHON_9.3", "")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "BCR_NAME", '"' + region.upper() + '"', "PYTHON_9.3", "")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "HABITAT_TY", "Calc( !HABITAT_TY!)", "PYTHON_9.3", "def Calc(hab):\\n  if (hab.lower() == 'woody vegetation'):\\n    return 'Woody wetlands'\\n  else:\\n    return hab")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "HABITAT_TY", "Calc( !HABITAT_TY!)", "PYTHON_9.3", "def Calc(hab):\\n  return hab.lower()")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "COVER_TYPE", "Calc( !COVER_TYPE!)", "PYTHON_9.3", "def Calc(cover):\\n  cover = cover.lower()\\n  changeme = ['reforested', 'forested swamp', 'hardwoods']\\n  if cover in changeme:\\n    return 'woody wetlands'\\n  else:\\n    return cover")
        # Add geometry
        print "Adding geometry"
        pubfloodaoi = arcpy.AddGeometryAttributes_management(pubfloodaoi, "AREA", "", "ACRES", "PROJCS['WGS_1984_UTM_Zone_15N',GEOGCS['GCS_WGS_1984',DATUM['D_WGS_1984',SPHEROID['WGS_1984',6378137.0,298.257223563]],PRIMEM['Greenwich',0.0],UNIT['Degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['False_Easting',500000.0],PARAMETER['False_Northing',0.0],PARAMETER['Central_Meridian',-93.0],PARAMETER['Scale_Factor',0.9996],PARAMETER['Latitude_Of_Origin',0.0],UNIT['Meter',1.0]]")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "ACRES", "!POLY_AREA!", "PYTHON_9.3", "")
        pubfloodaoi = arcpy.CalculateField_management(pubfloodaoi, "HECTARES", "!ACRES!*0.404686", "PYTHON_9.3", "")
        # Export final output
        print "Exporting finished dataset"
        arcpy.FeatureClassToFeatureClass_conversion(pubfloodaoi, gdb, region + "_Output_" + datetime.datetime.now().strftime('%m_%d_%Y'),"" ,'STATE \"STATE\" true true false 18 Text 0 0 ,First,#,in_memory/selectt,STATE,-1,-1;BCR_NAME \"BCR_NAME\" true true false 27 Text 0 0 ,First,#,in_memory/selectt,BCR_NAME,-1,-1;ACRES \"ACRES\" true true false 19 Double 3 18 ,First,#,in_memory/selectt,ACRES,-1,-1;HECTARES \"HECTARES\" true true false 19 Double 3 18 ,First,#,in_memory/selectt,HECTARES,-1,-1;Z_RED_OAK_ \"Z_RED_OAK_\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Z_RED_OAK_,-1,-1;HABITAT_TY \"HABITAT_TY\" true true false 16 Text 0 0 ,First,#,in_memory/selectt,HABITAT_TY,-1,-1;Z_HARVESTE \"Z_HARVESTE\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Z_HARVESTE,-1,-1;MANAGING_A \"MANAGING_A\" true true false 254 Text 0 0 ,First,#,in_memory/selectt,MANAGING_A,-1,-1;COMMON_NAM \"COMMON_NAM\" true true false 100 Text 0 0 ,First,#,in_memory/selectt,COMMON_NAM,-1,-1;MANAGEMENT \"MANAGEMENT\" true true false 100 Text 0 0 ,First,#,in_memory/selectt,MANAGEMENT,-1,-1;FUNCTIONAL \"FUNCTIONAL\" true true false 1 Text 0 0 ,First,#,in_memory/selectt,FUNCTIONAL,-1,-1;OWNER \"OWNER\" true true false 20 Text 0 0 ,First,#,in_memory/selectt,OWNER,-1,-1;COVER_TYPE \"COVER_TYPE\" true true false 254 Text 0 0 ,First,#,in_memory/selectt,COVER_TYPE,-1,-1;MANAGE \"MANAGE\" true true false 5 Short 0 5 ,First,#,in_memory/selectt,MANAGE,-1,-1;BASIN__HUC \"BASIN__HUC\" true true false 29 Text 0 0 ,First,#,in_memory/selectt,BASIN__HUC,-1,-1;PROTECTION \"PROTECTION\" true true false 20 Text 0 0 ,First,#,in_memory/selectt,PROTECTION,-1,-1;SEEDINDEX \"SEEDINDEX\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,SEEDINDEX,-1,-1;WTRCNTRL \"WTRCNTRL\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,WTRCNTRL,-1,-1;PUMP \"PUMP\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,PUMP,-1,-1;REF_HAB \"REF_HAB\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,REF_HAB,-1,-1;REFHABAC \"REFHABAC\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,REFHABAC,-1,-1;DEDCALC \"DEDCALC\" true true false 13 Float 0 0 ,First,#,in_memory/selectt,DEDCALC,-1,-1;Shape_Leng \"Shape_Leng\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Shape_Leng,-1,-1;Shape_Area \"Shape_Area\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Shape_Area,-1,-1;FIXID \"FIXID\" true true false 10 Long 0 10 ,First,#,in_memory/selectt,FIXID,-1,-1', "")
        arcpy.TableToTable_conversion(pubfloodaoi, workspace, region + "_Output_" + datetime.datetime.now().strftime('%m_%d_%Y'),"", 'STATE \"STATE\" true true false 18 Text 0 0 ,First,#,in_memory/selectt,STATE,-1,-1;BCR_NAME \"BCR_NAME\" true true false 27 Text 0 0 ,First,#,in_memory/selectt,BCR_NAME,-1,-1;ACRES \"ACRES\" true true false 19 Double 3 18 ,First,#,in_memory/selectt,ACRES,-1,-1;HECTARES \"HECTARES\" true true false 19 Double 3 18 ,First,#,in_memory/selectt,HECTARES,-1,-1;Z_RED_OAK_ \"Z_RED_OAK_\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Z_RED_OAK_,-1,-1;HABITAT_TY \"HABITAT_TY\" true true false 16 Text 0 0 ,First,#,in_memory/selectt,HABITAT_TY,-1,-1;Z_HARVESTE \"Z_HARVESTE\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Z_HARVESTE,-1,-1;MANAGING_A \"MANAGING_A\" true true false 254 Text 0 0 ,First,#,in_memory/selectt,MANAGING_A,-1,-1;COMMON_NAM \"COMMON_NAM\" true true false 100 Text 0 0 ,First,#,in_memory/selectt,COMMON_NAM,-1,-1;MANAGEMENT \"MANAGEMENT\" true true false 100 Text 0 0 ,First,#,in_memory/selectt,MANAGEMENT,-1,-1;FUNCTIONAL \"FUNCTIONAL\" true true false 1 Text 0 0 ,First,#,in_memory/selectt,FUNCTIONAL,-1,-1;OWNER \"OWNER\" true true false 20 Text 0 0 ,First,#,in_memory/selectt,OWNER,-1,-1;COVER_TYPE \"COVER_TYPE\" true true false 254 Text 0 0 ,First,#,in_memory/selectt,COVER_TYPE,-1,-1;MANAGE \"MANAGE\" true true false 5 Short 0 5 ,First,#,in_memory/selectt,MANAGE,-1,-1;BASIN__HUC \"BASIN__HUC\" true true false 29 Text 0 0 ,First,#,in_memory/selectt,BASIN__HUC,-1,-1;PROTECTION \"PROTECTION\" true true false 20 Text 0 0 ,First,#,in_memory/selectt,PROTECTION,-1,-1;SEEDINDEX \"SEEDINDEX\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,SEEDINDEX,-1,-1;WTRCNTRL \"WTRCNTRL\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,WTRCNTRL,-1,-1;PUMP \"PUMP\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,PUMP,-1,-1;REF_HAB \"REF_HAB\" true true false 5 Text 0 0 ,First,#,in_memory/selectt,REF_HAB,-1,-1;REFHABAC \"REFHABAC\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,REFHABAC,-1,-1;DEDCALC \"DEDCALC\" true true false 13 Float 0 0 ,First,#,in_memory/selectt,DEDCALC,-1,-1;Shape_Leng \"Shape_Leng\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Shape_Leng,-1,-1;Shape_Area \"Shape_Area\" true true false 19 Double 0 0 ,First,#,in_memory/selectt,Shape_Area,-1,-1;FIXID \"FIXID\" true true false 10 Long 0 10 ,First,#,in_memory/selectt,FIXID,-1,-1', "")
        print "Everything looks good"
	sys.exit()
