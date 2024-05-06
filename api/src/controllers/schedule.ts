/**
 @module ScheduleRoute
*/

import { publicProcedure, router } from '../helpers/trpc';
import typia from 'typia';

const TERM_SEASONS = ['Winter', 'Spring', 'Summer1', 'Summer10wk', 'Summer2', 'Fall'];

const callPPAPIWebSoc = async (params: Record<string, string>) => {
  const url: URL = new URL(process.env.PUBLIC_API_URL + 'websoc?' + new URLSearchParams(params));
  return await fetch(url)
    .then((response) => response.json())
    .then((json) => json.payload);
};

const scheduleRouter = router({
  /**
   * Get terms from years
   */
  getTerms: publicProcedure.input(typia.createAssert<{ years: string }>()).query(async ({ input }) => {
    let pastYears: number = parseInt(input.years);
    if (!pastYears) {
      pastYears = 1;
    }
    const d = new Date();
    const year = d.getFullYear();
    const terms = [];
    for (let y = year - pastYears; y <= year; ++y) {
      for (let i = 0; i < TERM_SEASONS.length; ++i) {
        terms.push(`${y} ${TERM_SEASONS[i]}`);
      }
    }
    return terms;
  }),

  /**
   * Get the current week
   */
  currentWeek: publicProcedure.query(async () => {
    const apiResp = await fetch(`${process.env.PUBLIC_API_URL}week`);
    const json = await apiResp.json();
    return json.payload;
  }),

/**
 * Get the current quarter on websoc
 */
router.get('/api/currentQuarter', async function (_, res) {
  const apiResp = await fetch(`${process.env.PUBLIC_API_URL}websoc/terms`);
  const json = await apiResp.json();
  res.send(json.payload[0]);
});

  /**
   * Proxy for WebSOC, using PeterPortal API
   */
  getTermDeptNum: publicProcedure
    .input(typia.createAssert<{ term: string; department: string; number: string }>())
    .query(async ({ input }) => {
      const [year, quarter] = input.term.split(' ');
      const result = await callPPAPIWebSoc({
        year,
        quarter,
        department: input.department,
        courseNumber: input.number,
      });
      return result;
    }),

  /**
   * Proxy for WebSOC, using PeterPortal API
   */
  getTermProf: publicProcedure
    .input(typia.createAssert<{ term: string; professor: string }>())
    .query(async ({ input }) => {
      const [year, quarter] = input.term.split(' ');
      const result = await callPPAPIWebSoc({
        year,
        quarter,
        instructorName: input.professor,
      });
      return result;
    }),
});

export default scheduleRouter;
