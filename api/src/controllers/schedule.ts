/**
 @module ScheduleRoute
*/

import { z } from 'zod';
import { publicProcedure, router } from '../helpers/trpc';
import { TermResponse, WebsocAPIResponse, WeekData } from '@peterportal/types';

const callPPAPIWebSoc = async (params: Record<string, string>) => {
  const url: URL = new URL(process.env.PUBLIC_API_URL + 'websoc?' + new URLSearchParams(params));
  return await fetch(url)
    .then((response) => response.json())
    .then((json) => json.payload as WebsocAPIResponse);
};

const scheduleRouter = router({
  /**
   * Get the current week
   */
  currentWeek: publicProcedure.query(async () => {
    const apiResp = await fetch(`${process.env.PUBLIC_API_URL}week`);
    const json = await apiResp.json();
    return json.payload as WeekData;
  }),

  /**
   * Get the current quarter on websoc
   */
  currentQuarter: publicProcedure.query(async () => {
    const apiResp = await fetch(`${process.env.PUBLIC_API_URL}websoc/terms`);
    const json = await apiResp.json();
    return (json.payload as TermResponse)[0].shortName;
  }),

  /**
   * Proxy for WebSOC, using PeterPortal API
   */
  getTermDeptNum: publicProcedure
    .input(z.object({ term: z.string(), department: z.string(), number: z.string() }))
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
  getTermProf: publicProcedure.input(z.object({ term: z.string(), professor: z.string() })).query(async ({ input }) => {
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
