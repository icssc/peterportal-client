import fetch from 'node-fetch';
import cheerio from 'cheerio';
import { COLLECTION_NAMES, setValue, getValue } from './mongo';

export function getCurrentQuarter() {
    return new Promise(async (resolve, reject) => {
        // check if is in the cache
        let cacheKey = `currentQuarter`
        let cacheValue = await getValue(COLLECTION_NAMES.SCHEDULE, cacheKey)
        if (cacheValue) {
            // how many days since last update
            let age = Math.round(((new Date()).getTime() - cacheValue.date) / (1000 * 60 * 60 * 24));
            // use cached value if within a day old
            if (age < 1) {
                resolve(cacheValue.value);
                return;
            }
        }

        // get websoc page
        let url = `https://www.reg.uci.edu/perl/WebSoc`;
        let res = await fetch(url);
        let text = await res.text();
        // scrape websoc
        let $ = cheerio.load(text);
        // get the quarter dropdown
        let selects = $('select[name="YearTerm"]').toArray();
        if (selects.length > 0) {
            // get the first item
            let firstQuarter = $(selects[0]).find('option').first();
            if (firstQuarter) {
                // get the text in the selected item
                let currentQuarter = $(firstQuarter).text();
                // cache the value and the date acquired
                setValue(COLLECTION_NAMES.SCHEDULE, cacheKey, {
                    date: new Date(),
                    value: currentQuarter
                });
                resolve(currentQuarter);
                return;
            }
        }
        reject('');
    })
}