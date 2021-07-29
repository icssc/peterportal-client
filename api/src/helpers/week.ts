import fetch from 'node-fetch';
import cheerio, { CheerioAPI, Element } from 'cheerio';
import { COLLECTION_NAMES, containsID, getDocuments, addDocument, updateDocument } from './mongo';
import { QuarterMapping, WeekData } from '../types/types';

// CACHE FUNCTIONS 
// retrieve cached value from key
async function getValue(key: string): Promise<any> {
    return new Promise(async resolve => {
        let value = await getDocuments(COLLECTION_NAMES.SCHEDULE, { _id: key });
        // cache hit
        if (value.length > 0) {
            resolve(value[0]["value"]);
        }
        // cache miss
        else {
            resolve(undefined);
        }
    })
}

// cache a value given a key
async function setValue(key: string, value: any): Promise<void> {
    return new Promise(async resolve => {
        // if already in cache, update doc
        if (await containsID(COLLECTION_NAMES.SCHEDULE, key)) {
            await updateDocument(COLLECTION_NAMES.SCHEDULE, { _id: key }, { value: value })
        }
        // if not in cache, add doc
        else {
            await addDocument(COLLECTION_NAMES.SCHEDULE, { _id: key, value: value })
        }
        resolve();
    })
}

// main function (only function to export)
export function getWeek(): Promise<WeekData> {
    return new Promise(async resolve => {
        // current date
        let date = new Date(Date.now());
        // current year
        let year = date.getFullYear();

        // check for current year to current year + 1
        let quarterMapping1 = await getQuarterMapping(year) as QuarterMapping;
        let potentialWeek = findWeek(date, quarterMapping1);
        // if the date lies within this page
        if (potentialWeek) {
            resolve(potentialWeek);
            return;
        }

        // check for current year - 1 to current year
        let quarterMapping2 = await getQuarterMapping(year - 1) as QuarterMapping;
        potentialWeek = findWeek(date, quarterMapping2);
        if (potentialWeek) {
            resolve(potentialWeek);
        }
        else {
            // date not in any school term, probably in break
            resolve({
                week: 0,
                quarter: 'Break Time',
                display: "Enjoy your break!",
            });
        }
    })
}

// date: current date
// quarterMapping: the quarterMapping
// returns the week description if it lies within the quarter mapping
function findWeek(date: Date, quarterMapping: QuarterMapping): WeekData {
    let result: WeekData = undefined!;
    // iterate through each quarter
    Object.keys(quarterMapping).forEach(function (quarter) {
        let begin = new Date(quarterMapping[quarter]["begin"])
        let end = new Date(quarterMapping[quarter]["end"])
        // check if the date lies within the start/end range
        if (date >= begin && date <= end) {
            let week = Math.floor(dateSubtract(begin, date) / 7) + 1;
            let display = `Week ${week}, ${quarter}`
            result = {
                week: week,
                quarter: quarter,
                display: display
            }
        }
        // check if date is 1 week after end
        else if (date > end && date <= addDays(end, 7)) {
            let display = `Finals Week, ${quarter}. Good Luck!🤞`
            result = {
                week: 11,
                quarter: quarter,
                display: display
            }
        }
    });
    return result;
}

// year: the academic year to search for
// quarterMapping: {quarter:{start:Date, end:Date}}
// given a year, get quarter to date range mapping
async function getQuarterMapping(year: number): Promise<QuarterMapping> {
    return new Promise(async resolve => {
        // check if is in the cache
        let cacheKey = `quarterMapping${year}`
        let cacheValue = await getValue(cacheKey)
        if (cacheValue) {
            resolve(cacheValue);
            return;
        }
        // maps quarter description to day range
        let quarterToDayMapping = {}
        // url to academic calendar
        let url = `https://reg.uci.edu/calendars/quarterly/${year}-${year + 1}/quarterly${year % 100}-${(year % 100) + 1}.html`
        let res = await fetch(url);
        let text = await res.text();
        // scrape the calendar
        let $ = cheerio.load(text);
        // load all tables on the page
        let tables = $('table[class="calendartable"]').toArray()
        // process each table
        tables.forEach(table => {
            processTable(table, $, quarterToDayMapping, year);
        })
        await setValue(cacheKey, quarterToDayMapping);
        resolve(quarterToDayMapping);
    })
}

// finds the table labels and process each row
// table: the cherio table element
// $: cherio command
// quarterToDayMapping: the mapping to store data into
// year: the beginning academic year
// finds and assigns the beginning and end date for a table
function processTable(table: Element, $: CheerioAPI, quarterToDayMapping: QuarterMapping, year: number) {
    // find the tbody
    let tbody = $(table).find('tbody');
    // reference all rows in the table
    let rows = tbody.find("tr").toArray() as Element[];
    // the first row has all the labels for the table
    let tableLabels = $(rows[0]).find("td").toArray() as Element[];
    rows.forEach(row => {
        // process each row
        processRow(row, $, quarterToDayMapping, tableLabels, year)
    });
}

// process a row
// row: the cherio row element
// $: cherio command
// quarterToDayMapping: the mapping to store data into
// tableLabels: the cherio table label element
// year: the beginning academic year
// checks if is a row is a beginning or end date
function processRow(row: Element, $: CheerioAPI, quarterToDayMapping: QuarterMapping, tableLabels: Element[], year: number) {
    // get all information from row
    let rowInfo = $(row).find("td").toArray()
    // start date
    if ($(rowInfo[0]).text() == "Instruction begins") {
        // for each season
        for (let i = 1; i < 4; i++) {
            let dateEntry = $(rowInfo[i]).text()
            let dateLabel = strip($(tableLabels[i]).text());
            quarterToDayMapping[dateLabel] = { "begin": processDate(dateEntry, dateLabel, year), "end": new Date() };
        }
    }
    // end date
    else if ($(rowInfo[0]).text() == "Instruction ends") {
        // for each season
        for (let i = 1; i < 4; i++) {
            let dateEntry = $(rowInfo[i]).text()
            let dateLabel = strip($(tableLabels[i]).text());
            quarterToDayMapping[dateLabel]["end"] = processDate(dateEntry, dateLabel, year);
        }
    }
}

// dateEntry: a date entry on the calendar (eg. Jan 17)
// dateLabel: a date label on the calendar (eg. Winter 2020 or Summer Session 10WK)
// year: the beginning academic year (eg. 2019)
// returns a Date for the corresponding table entry
function processDate(dateEntry: string, dateLabel: string, year: number): Date {
    let splitDateEntry = dateEntry.split(" ");
    let month = splitDateEntry[0];
    let day = splitDateEntry[1];
    let labelYear = dateLabel.split(" ")[1];
    let correctYear = isInteger(labelYear) ? labelYear : year + 1;
    return new Date(`${month}/${day}/${correctYear}`)
}

// helper functions
function strip(str: string): string {
    return str.replace(/^\s+|\s+$/g, '');
}

// if is integer or not
function isInteger(num: string): boolean {
    return !isNaN(parseInt(num, 10))
}

// get the number of days between two dates
function dateSubtract(date1: Date, date2: Date): number {
    // To calculate the time difference of two dates 
    let Difference_In_Time = date2.getTime() - date1.getTime();
    // To calculate the no. of days between two dates 
    return Difference_In_Time / (1000 * 3600 * 24);
}

// add days to a date
function addDays(date: Date, days: number): Date {
    date.setDate(date.getDate() + days);
    return date;
}