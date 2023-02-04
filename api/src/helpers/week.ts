/**
 @module WeekHelper
*/

import fetch from 'node-fetch';
import cheerio, { CheerioAPI, Element } from 'cheerio';
import { COLLECTION_NAMES, setValue, getValue } from './mongo';
import { QuarterMapping, WeekData } from '../types/types';

/**
 * Get the current week and quarter. A display string is also provided.
 */
export function getWeek(): Promise<WeekData> {
    return new Promise(async resolve => {
        // current date
        let date = new Date(Date.now());
        date = new Date(2022, 9, 22);
        console.log(date);
        // current year
        let year = date.getFullYear();

        // check for current year to current year + 1
        let quarterMapping1 = await getQuarterMapping(year) as QuarterMapping;
        console.log(quarterMapping1);
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
                week: -1,
                quarter: 'N/A',
                display: 'Enjoy your break!😎',
            });
        }
    })
}

/**
 * 
 * @param date Today's date
 * @param quarterMapping Maps a quarter to its start and end date 
 * @returns Week description if it lies within the quarter
 */
function findWeek(date: Date, quarterMapping: QuarterMapping): WeekData {
    let result: WeekData = undefined!;
    // iterate through each quarter
    Object.keys(quarterMapping).forEach(function (quarter) {
        let begin = new Date(quarterMapping[quarter]['begin'])
        let end = new Date(quarterMapping[quarter]['end'])

        let isFallQuarter = false;
        // in fall quarter, instruction start date is not on a monday
        // it is on a thursday in week 0
        // let's move it back to monday in week 0 and adjust our week calculations by -1
        // that way week calculations are correct for fall quarter
        // note getUTCDay and setUTCDay are used because the dates received from the mongo
        // cache are in UTC, e.g. Mon Jan 9, 2023 00:00 UTC for Winter 2023 start
        // using regular getDay and setDay can result in the wrong day being returned if
        // the prod or dev server is not in UTC time (it's not, unless you're developing in England right now)
        if (begin.getUTCDay() !== 1) {
            isFallQuarter = true;
            begin.setUTCDate(begin.getUTCDate() - 3);
        }

        // begin/end dates retrieved from mongo are currently in UTC
        // so for example, Winter 2023 starts on Monday Jan 9, 2023 PST
        // the date retrieved from mongo is incorrectly Jan 9, 2023 00:00 UTC, when it should be
        // Jan 8, 2023 17:00 UTC (since Irvine is in PST which is 8 hours behind UTC)
        // we want to fix this offset for accurate comparsions
        // it should compare accurately regardless of whatever time zone the prod or development server is in
        console.log(begin.toUTCString());
        if (begin.getUTCHours() === 0) {
            // TODO: need to detect if the date is in PDT instead of PST, would only add 7 if its in PDT
            // TODO: probably a better way to do this exists
            begin.setUTCHours(begin.getUTCHours() + 8);
        }

        // check if the date lies within the start/end range
        if (date >= begin && date <= end) {
            let week = Math.floor(dateSubtract(begin, date) / 7) + (isFallQuarter ? 0 : 1); // if it's fall quarter, start counting at week 0, otherwise 1
            let display = `Week ${week} • ${quarter}`
            result = {
                week: week,
                quarter: quarter,
                display: display
            }
        }
        // check if date is 1 week after end
        else if (date > end && date <= addDays(end, 7)) {
            let display = `Finals Week • ${quarter}. Good Luck!🤞`
            result = {
                week: -1,
                quarter: quarter,
                display: display
            }
        }
    });
    return result;
}

/**
 * Given a year, get quarter to date range mapping
 * @param year Academic year to search for
 * @returns Mapping of quarters to its start and end date 
 */
async function getQuarterMapping(year: number): Promise<QuarterMapping> {
    return new Promise(async resolve => {
        // check if is in the cache
        let cacheKey = `quarterMapping${year}`
        let cacheValue = await getValue(COLLECTION_NAMES.SCHEDULE, cacheKey)
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
        await setValue(COLLECTION_NAMES.SCHEDULE, cacheKey, quarterToDayMapping);
        resolve(quarterToDayMapping);
    })
}

/**
 * Parses the quarter names from table labels and processes each row to find the start and end dates
 * @param table Cherio table element
 * @param $ Cherio command
 * @param quarterToDayMapping Mapping to store data into
 * @param year Beginning academic year
 */
function processTable(table: Element, $: CheerioAPI, quarterToDayMapping: QuarterMapping, year: number) {
    // find the tbody
    let tbody = $(table).find('tbody');
    // reference all rows in the table
    let rows = tbody.find('tr').toArray() as Element[];
    // the first row has all the labels for the table
    let tableLabels = $(rows[0]).find('td').toArray() as Element[];
    rows.forEach(row => {
        // process each row
        processRow(row, $, quarterToDayMapping, tableLabels, year)
    });
}

/**
 * Checks if a row contains info on beginning or end date
 * @param row Cherio row element
 * @param $ Cherio command
 * @param quarterToDayMapping Mapping to store data into
 * @param tableLabels Column labels in the current table 
 * @param year Beginning academic year
 */
function processRow(row: Element, $: CheerioAPI, quarterToDayMapping: QuarterMapping, tableLabels: Element[], year: number) {
    // get all information from row
    let rowInfo = $(row).find('td').toArray()
    // start date
    if ($(rowInfo[0]).text() == 'Instruction begins') {
        // for each season
        for (let i = 1; i < 4; i++) {
            let dateEntry = $(rowInfo[i]).text()
            let dateLabel = strip($(tableLabels[i]).text());
            quarterToDayMapping[dateLabel] = { 'begin': processDate(dateEntry, dateLabel, year), 'end': new Date() };
        }
    }
    // end date
    else if ($(rowInfo[0]).text() == 'Instruction ends') {
        // for each season
        for (let i = 1; i < 4; i++) {
            let dateEntry = $(rowInfo[i]).text()
            let dateLabel = strip($(tableLabels[i]).text());
            quarterToDayMapping[dateLabel]['end'] = processDate(dateEntry, dateLabel, year);
        }
    }
}

/**
 * Form a date object based on data from the calendar
 * @example 
 * // returns Date(1/17/2020)
 * processDate('Jan 17', 'Winter 2020', 2019)
 * @example 
 * // returns Date(7/30/2021)
 * processDate('July 30', 'Summer Session 10WK', 2020)
 * @param dateEntry Date entry on the calendar
 * @param dateLabel Date label on the calendar
 * @param year Beginning academic year
 * @returns Date for the corresponding table entry
 */
function processDate(dateEntry: string, dateLabel: string, year: number): Date {
    let splitDateEntry = dateEntry.split(' ');
    let month = splitDateEntry[0];
    let day = splitDateEntry[1];
    let labelYear = dateLabel.split(' ')[1];
    // 'Winter 2020' => 2020, but 'Summer Session I' => Session
    // Exception for Summer Session
    let correctYear = isInteger(labelYear) ? labelYear : year + 1;
    return new Date(`${month}/${day}/${correctYear}`)
}

/**
 * Remove trailing/leading whitespace from string
 * @param str Original string
 * @returns New string with whitespace removed
 */
function strip(str: string): string {
    return str.replace(/^\s+|\s+$/g, '');
}

/**
 * Determine if a number is an integer or not
 * @param num Number to test
 * @returns True if is an integer
 */
function isInteger(num: string): boolean {
    return !isNaN(parseInt(num, 10))
}

/**
 * Get the number of days between two dates
 * @param date1 Earlier date
 * @param date2 Later date
 * @returns Number of days between date1 and date2
 */
function dateSubtract(date1: Date, date2: Date): number {
    // To calculate the time difference of two dates 
    let Difference_In_Time = date2.getTime() - date1.getTime();
    // To calculate the no. of days between two dates 
    return Difference_In_Time / (1000 * 3600 * 24);
}

/**
 * Add days to a date
 * @param date Date to add days to
 * @param days Number of days to add
 * @returns Same date as the one passed in
 */
function addDays(date: Date, days: number): Date {
    date.setDate(date.getDate() + days);
    return date;
}