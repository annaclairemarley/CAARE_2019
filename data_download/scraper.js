const puppeteer = require('puppeteer');
const fs = require('fs');

regions = {
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
     * @param {string} startDate Of format yyyy-mm-dd
     * @param {string} endDate   Of format yyyy-mm-dd
     * @param {string} variable  A string that is: pdsi, swe or snow_Depth (snow depth)
     * @param {string} region    Name of region. Must be in regions list
     * @param {bool}   isChapter A boolean if the region name is a chapter name (True) or not (False)
     */
    async function getDataHelper(startDate, endDate, variable, regionName, isChapter){
        productTS = "G";
        variableTS = "pr";
        scaleTS = 4000;
        regionName = encodeURIComponent(regionName)
        statisticTS = "Total"
        geom_geoms = "projects/climate-engine/featureCollections/shp_simplified/ClimateEngine_Navajo_Nation_Agencies"
        geom_regions = "navajo_nation_agencies"
        if (isChapter === true) {
            geom_geoms = "projects/climate-engine/featureCollections/shp_simplified/ClimateEngine_Navajo_Nation_Chapters"
            geom_regions = "navajo_nation_chapters"
        }
        switch(variable) {
            case "pdsi":
                productTS = "G";
                variableTS = "pdsi";
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
        }
        
        const page = await browser.newPage();
        await page.goto('https://app.climateengine.org/?toolAction=getTimeSeriesOverDateRange&timeSeriesCalc=days&variable2display=none&productTypeTS=MET' + 
                        '&productTS=' + productTS + 
                        '&variableTS=' + variableTS + 
                        '&statisticTS=' + statisticTS + 
                        '&scaleTS=' + scaleTS + 
                        '&unitsTS=metric' +
                        '&dateStartTS=' + startDate + 
                        '&dateEndTS=' + endDate + 
                        '&mapCenterLongLat=-109.7906%2C36.2531&mapzoom=8&chartType=line&runningMeanDays=9&' +
                        'geom_geoms=' + geom_geoms + 
                        '&geom_types=navajo_nation&geom_meta_types=feature_collection&geom_displays=block&geom_checks=checked' + 
                        '&geom_altnames=' + regionName +
                        '&geom_columnnames=Name' + 
                        '&geom_regions=' + geom_regions +
                        '&geom_subchoices=' + regionName);

        // Get the "viewport" of the page, as reported by the page.
        const csv = await page.evaluate(() => {
            return myChart.getCSV(true);
        });

        page.close();

        return csv.split("\n");
    };

    /**
     * Checks size of differenc ein 3 month intervals between dates
     * @param {Date} dt2 
     * @param {Date} dt1 
     */
    function diff_6months(dt2, dt1) {

        var diff =(dt2.getTime() - dt1.getTime()) / 1000;
        diff /= (60 * 60 * 24);
        return Math.abs(Math.round(diff/180));
    
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
     * @param {bool} useChapters    If to use chapters (true) or agencies (false)
     * @param {string} startDate    The start date for data in format yyyy-mm-dd
     * @param {string} endDate      The end date for the data in format yyy-mm-dd (Inclusive)
     * @param {string} variable     The variable to get. Can be one of: swe, snow_depth or pdsi
     * @param {string} customHeader [Optional] String to use as custom header for csv file. 
     */
    async function getData(useChapters, startDate, endDate, variable, customHeader) {
        customHeader = customHeader || null;
        //Splt date into 6 month intervals
        timeSegments = []
        sDate = toDate(startDate);
        eDate = toDate(endDate);
        currentEndDate = toDate(startDate);
        currentEndDate.setDate(currentEndDate.getDate() + 180);
        if (diff_6months(eDate, sDate) < 1 ) {
            timeSegments.push([formatDate(sDate), formatDate(eDate)]);
        } else {
            while (currentEndDate != eDate) {
                if (currentEndDate > eDate) {
                    currentEndDate = eDate;
                }
                timeSegments.push([formatDate(sDate), formatDate(currentEndDate)]);
                sDate = new Date(currentEndDate.getTime());
                sDate.setDate(sDate.getDate() + 1);
                currentEndDate.setDate(currentEndDate.getDate() + 180);
            }
        }
        //Select regions
        regionsToUse = regions.agencies;
        if (useChapters) {
            regionsToUse = regions.chapters;
        }
        
        for (regionIndex in regionsToUse) {
            data = []
            header = ""
            for (timeframeIndex in timeSegments) {
                let csv = await getDataHelper(timeSegments[timeframeIndex][0], timeSegments[timeframeIndex][1], variable, regionsToUse[regionIndex], useChapters);
                header = csv.shift(); //Remove header
                data.push(...csv);
            }
        
            if (customHeader !== null) {
                header = customHeader;
            }

            fs.writeFile("data/" + regionsToUse[regionIndex] + "_" + variable + "_" + startDate + "_" + endDate + ".csv", header + "\n" + data.join("\n"), function(err) {
                if(err) {
                    return console.log(err);
                }
            
                console.log(regionsToUse[regionIndex] + "_" + variable + "_" + startDate + "_" + endDate + ".csv Saved!");
            }); 

            
        }
        
    }

    getData(true, "2003-09-01", "2019-07-17", "pdsi", "Date,Palmer Drounght Severity Index").then(() => {
        browser.close();
    }).catch(e => console.log(e));
    // getDataHelper("2018-03-15", "2018-04-15", "swe", regions.agencies[2], false).then(csv => {
    //     console.log(csv);
    // })
    // getDataHelper("2018-01-15", "2018-02-15", "swe", regions.agencies[2], false).then(csv => {
    //     console.log(csv);
    // })
    
});