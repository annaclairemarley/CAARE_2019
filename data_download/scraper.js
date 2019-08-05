const puppeteer = require('puppeteer');
const fs = require('fs');

const TIME_CHUNK_SIZE = 90;
const TIMEOUT_SECONDS = 60;
const regionType = {
    AGENCY: 'a',
    CHAPTER: 'c',
    FUSION_TABLE: 'ft'
}
const regions = {
    "agencies": [
        "CHINLE",
        "EASTERN NAVAJO",
        "FORT DEFIANCE",
        "SHIPROCK",
        "WESTERN NAVAJO"],
    "chapters": [
        "ALAMO",
        "ANETH",
        "BACA/PREWITT",
        "BECENTI",
        "BECLAHBITO",
        "BIRD SPRINGS",
        "BLACK MESA",
        "BODAWAY/GAP",
        "BREAD SPRINGS",
        "BURNHAM",
        "CAMERON",
        "CASAMERO LAKE",
        "CHICHILTAH",
        "CHILCHINBITO",
        "CHINLE",
        "CHURCH ROCK",
        "COALMINE MEZA",
        "COPPER MINE",
        "CORNFIELDS",
        "COUNSELOR",
        "COVE",
        "COYOTE CANYON",
        "CROWNPOINT",
        "CRYSTAL",
        "DENNEHOTSO",
        "DILKON",
        "FOREST LAKE",
        "FORT DEFIANCE",
        "GADIIAHI",
        "GANADO",
        "HARDROCK",
        "HOGBACK",
        "HOUCK",
        "HUERFANO",
        "INDIAN WELLS",
        "INSCRIPTION HOUSE",
        "IYANBITO",
        "JEDDITO",
        "KAIBITO",
        "KAYENTA",
        "KINLICHEE",
        "KLAGETOH",
        "LAKE VALLEY",
        "LECHEE",
        "LEUPP",
        "LITTLEWATER",
        "LOW MOUNTAIN",
        "LOWER GREASEWOOD",
        "LUKACHUKAI",
        "LUPTON",
        "MANUELITO",
        "MANY FARMS",
        "MARIANO LAKE",
        "MEXICAN SPRINGS",
        "MEXICAN WATER",
        "NAGEEZI",
        "NAHATA'DZIL'",
        "NAHODISHGISH",
        "NASHITTI",
        "NAVAJO MOUNTAIN",
        "NAZLINI",
        "NENAHNEZAD",
        "NEWCOMB",
        "OAK SPRINGS",
        "OJO ENCINO",
        "OLJATO",
        "PEUBLO PINTADO",
        "PINEDALE",
        "PINON",
        "RAMAH",
        "RED LAKE",
        "RED MESA",
        "RED ROCK",
        "RED VALLEY",
        "ROCK POINT",
        "ROCK SPRINGS",
        "ROUGH ROCK",
        "ROUND ROCK",
        "SAINT MICHAELS",
        "SANOSTEE",
        "SAWMILL",
        "SHEEP SPRINGS",
        "SHIPROCK",
        "SHONTO",
        "SMITH LAKE",
        "STANDING ROCK",
        "STEAMBOAT",
        "SWEETWATER",
        "TACHEE",
        "TEEC NOS POS",
        "TEESTO",
        "THOREAU",
        "TOHAJIILEE",
        "TOHATCHI",
        "TOLANI LAKE",
        "TONALEA",
        "TORREON",
        "TSAILE/WHEATFIELDS",
        "TSAYATOH",
        "TSELANI",
        "TUBA CITY",
        "TWIN LAKES",
        "TWO GREY HILLS",
        "UPPER FRUITLAND",
        "WHIPPOORWILL",
        "WHITE ROCK",
        "WHITECONE",
        "WHITEHORSE LAKE",
        "WIDE RUINS"
    ]
}
puppeteer.launch({headless: false}).then(browser => {

    /**
     * Helper for extracting CSV data from websites.
     * @param {string} startDate  Of format yyyy-mm-dd
     * @param {string} endDate    Of format yyyy-mm-dd
     * @param {string} variable   A string that is: pdsi, swe or snow_Depth (snow depth)
     * @param {string} region     Name of region. Must be in regions list, or the name of a region in the given fusion table
     * @param {string} type       A regionType enum telling the type of the area 
     * @param {string} fusionID   If the regionType is FUSION_TABLE, this must be set to the fusion table ID
     */
    async function getDataHelper(startDate, endDate, variable, regionName, type, fusionID){
        productTS = "G";
        variableTS = "pr";
        scaleTS = 4000;
        regionName = encodeURIComponent(regionName);
        statisticTS = "Total";
        geom_geoms = "projects/climate-engine/featureCollections/shp_simplified/ClimateEngine_Navajo_Nation_Agencies";
        geom_regions = "navajo_nation_agencies";
        geom_types = "navajo_nation";
        geom_meta_types = "feature_collection";
        geom_columnnames = "Name";
        switch(type) {
            case regionType.AGENCY:
                break;
            case regionType.CHAPTER:
                geom_geoms = "projects/climate-engine/featureCollections/shp_simplified/ClimateEngine_Navajo_Nation_Chapters";
                geom_regions = "navajo_nation_chapters";
                break;
            case regionType.FUSION_TABLE:
                geom_geoms = fusionID;
                geom_types = "custom_fusiontable";
                geom_meta_types = "custom_fusiontable";
                geom_columnnames = "Name";
                geom_regions = "";

        }
        switch(variable) {
            case "pr":
                break;
            case "pdsi":
                productTS = "G";
                variableTS = "pdsi";
                scaleTS = 4000;
                statisticTS = ""
                break;
            case "spi":
                productTS = "G";
                variableTS = "spi";
                scaleTS = 4000;
                statisticTS = ""
                break;
            case "swe":
                productTS = "SNODAS";
                variableTS = "SWE";
                scaleTS = 1000;
                break;
            case "snow_depth":
                productTS = "SNODAS";
                variableTS = "Snow_Depth"
                scaleTS = 1000;
                break;
            case "CHIRPS_daily":
                scaleTS = 4800;
                productTS = "CHIRPS_DAILY";
                variableTS = "precipitation";
                statisticTS = "Total";
        }


        const page = await browser.newPage();
        await page.setDefaultNavigationTimeout(1000 * TIMEOUT_SECONDS); 
        console.log('https://app.climateengine.org/climateEngine?toolAction=getTimeSeriesOverDateRange&timeSeriesCalc=days&variable2display=none&productTypeTS=MET' + 
        '&productTS=' + productTS + 
        '&variableTS=' + variableTS + 
        '&statisticTS=' + statisticTS + 
        '&scaleTS=' + scaleTS + 
        '&unitsTS=metric' +
        '&dateStartTS=' + startDate + 
        '&dateEndTS=' + endDate + 
        '&mapCenterLongLat=-109.7906%2C36.2531&mapzoom=8&chartType=line&runningMeanDays=9' +
        '&geom_geoms=' + geom_geoms + 
        '&geom_types=' + geom_types +  
        '&geom_meta_types=' + geom_meta_types + 
        '&geom_displays=block&geom_checks=checked' + 
        '&geom_altnames=' + regionName +
        '&geom_columnnames=' + geom_columnnames + 
        '&geom_regions=' + geom_regions +
        '&geom_subchoices=' + regionName)
        let attempts = 0;
        while (attempts < 5) {
            try {
                await page.goto('https://app.climateengine.org/climateEngine?toolAction=getTimeSeriesOverDateRange&timeSeriesCalc=days&variable2display=none&productTypeTS=MET' + 
                        '&productTS=' + productTS + 
                        '&variableTS=' + variableTS + 
                        '&statisticTS=' + statisticTS + 
                        '&scaleTS=' + scaleTS + 
                        '&unitsTS=metric' +
                        '&dateStartTS=' + startDate + 
                        '&dateEndTS=' + endDate + 
                        '&mapCenterLongLat=-109.7906%2C36.2531&mapzoom=8&chartType=line&runningMeanDays=9' +
                        '&geom_geoms=' + geom_geoms + 
                        '&geom_types=' + geom_types +
                        '&geom_meta_types=' + geom_meta_types +
                        '&geom_displays=block&geom_checks=checked' + 
                        '&geom_altnames=' + regionName +
                        '&geom_columnnames=' + geom_columnnames + 
                        '&geom_regions=' + geom_regions +
                        '&geom_subchoices=' + regionName);
 
                         

                // Get the "viewport" of the page, as reported by the page.
                const csv = await page.evaluate(() => {
                    return myChart.getCSV(true);
                });

                return csv.split("\n");
            } catch (err) {
                if (err.name == "TimeoutError" && attempts < 4) {
                    console.log("Time out. Trying again")
                    attempts += 1
                } else {
                    throw err;
                }
            } finally {
                page.close();
            }
        }
    };

    /**
     * Checks size of difference, in CHUNK_SIZE of day intervals between dates.
     * @param {Date} dt2 
     * @param {Date} dt1 
     */
    function chunk_diff(dt2, dt1, chunk_size) {

        var diff =(dt2.getTime() - dt1.getTime()) / 1000;
        diff /= (60 * 60 * 24);
        return Math.abs(Math.round(diff/chunk_size));
    
    }


    /**
     * Formats a date to yyy-mm-dd format
     * @param {Date} date 
     */
    function formatDate(date) {
        var d = new Date(date),
            month = '' + (d.getMonth() + 1),
            day = '' + d.getDate(),
            year = d.getFullYear();

        if (month.length < 2) month = '0' + month;
        if (day.length < 2) day = '0' + day;

        return [year, month, day].join('-');
    }

    /**
     * Converts a yyyy-mm-dd string to a date without a timezone
     * @param {string} dateStr 
     */
    function toDate(dateStr) {
        components = dateStr.split("-")
        components[1] -= 1; //Subtract 1 from month as they are 0 based
        return new Date(...components);
    }

    /**
     * Gets data for chapters or agencies in the navajo nation and saves it in the data folder.
     * @param {array[string]} regionNames Array of region names to download data for. Names must be as they appear in the regions array
     * @param {string} type               The type of the region. Must be one of the enums of regionType
     * @param {string} startDate          The start date for data in format yyyy-mm-dd
     * @param {string} endDate            The end date for the data in format yyy-mm-dd (Inclusive)
     * @param {string} variable           The variable to get. Can be one of: swe, snow_depth or pdsi
     * @param {array[string]} fusionIDs   [Optional] If the regionType is fusion table, then this array must be of the same length as the regionNames array, with each entry giving the corresponding fusion table
     * @param {string} customHeader       [Optional] String to use as custom header for csv file. 
     */
    async function getData(regionNames, type, startDate, endDate, variable, fusionIDs, customHeader) {
        customHeader = customHeader || null;
        //Splt date into 3 month intervals
        if (type === regionType.FUSION_TABLE) {
            if (regionNames.length !== fusionIDs.length) {
                throw Error("Fusion IDs and region names mismatched and the region type was given as FUSION_TABLE")
            }
        }
        timeSegments = []
        sDate = toDate(startDate);
        eDate = toDate(endDate);
        currentEndDate = toDate(startDate);
        currentEndDate.setDate(currentEndDate.getDate() + TIME_CHUNK_SIZE);
        if (chunk_diff(eDate, sDate, TIME_CHUNK_SIZE) < 1 ) {
            timeSegments.push([formatDate(sDate), formatDate(eDate)]);
        } else {
            while (currentEndDate != eDate) {
                if (currentEndDate > eDate) {
                    currentEndDate = eDate;
                }
                timeSegments.push([formatDate(sDate), formatDate(currentEndDate)]);
                sDate = new Date(currentEndDate.getTime());
                sDate.setDate(sDate.getDate() + 1);
                currentEndDate.setDate(currentEndDate.getDate() + 30);
            }
        }
        //Select regions
        regionsToUse = regionNames;
        currentEndDate = null;
        for (regionIndex in regionsToUse) {
            data = []
            header = ""
            try {
                for (timeframeIndex in timeSegments) {
                    let csv = null;
                    if (type == regionType.FUSION_TABLE) {
                        csv = await getDataHelper(timeSegments[timeframeIndex][0], timeSegments[timeframeIndex][1], variable, regionsToUse[regionIndex], type, fusionIDs[regionIndex]);
                    } else {
                        csv = await getDataHelper(timeSegments[timeframeIndex][0], timeSegments[timeframeIndex][1], variable, regionsToUse[regionIndex], type);
                    }
                    currentEndDate = timeSegments[timeframeIndex][1];
                    header = csv.shift(); //Remove header
                    data.push(...csv);
                }
            
                if (customHeader !== null) {
                    header = customHeader;
                }
            } catch(err) {
                console.log(err);
                console.log("Saving early...");
                endDate = currentEndDate;
            } finally {
                fs.writeFile("data/" + encodeURIComponent(regionsToUse[regionIndex]) + "_" + variable + "_" + startDate + "_" + endDate + ".csv", header + "\n" + data.join("\n"), function(err) {
                    if(err) {
                        return console.log(err);
                    }
                
                    console.log(encodeURIComponent(regionsToUse[regionIndex]) + "_" + variable + "_" + startDate + "_" + endDate + ".csv Saved!");
                }); 
            }

            
        }
        
    }

    // getData(regions.agencies, false, "2003-09-01", "2019-07-17", "pdsi", "Date,Palmer Drounght Severity Index").then(() => {
    //     browser.close();
    // }).catch(e => console.log(e));
    // Alternate usage:
    
    // getData(regions.chapters, true, "2003-09-01", "2019-07-17", "pdsi", "Date,Palmer Drounght Severity Index").then(() => {
    //     browser.close();
    // }).catch(e => console.log(e));
    getData(["Chinle"], regionType.FUSION_TABLE,"2003-06-03", "2019-07-17", "CHIRPS_daily", ["1N3bhhM8UdDecnO82iTGGSHZip6pJTkhU1G_UZQd7"], "Date,Precipitation").then(() => {
        browser.close();
    }).catch(e => console.log(e));
    
    
});
