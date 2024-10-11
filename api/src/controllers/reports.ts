/**
 @module ReportsRoute
*/

import Report from '../models/report';
import { adminProcedure, publicProcedure, router } from '../helpers/trpc';
import { z } from 'zod';
import { reportData } from '../types/schemas';

const reportsRouter = router({
  /**
   * Get all reports
   */
  get: adminProcedure.query(async () => {
    const reports = await Report.find();
    return reports;
  }),
  /**
   * Add a report
   */
  add: publicProcedure.input(reportData).mutation(async ({ input }) => {
    const report = new Report(input);
    await report.save();

    return input;
  }),
  /**
   * Delete a report
   */
  delete: adminProcedure
    .input(z.object({ id: z.string().optional(), reviewID: z.string().optional() }))
    .mutation(async ({ input }) => {
      if (input.id) {
        // delete report by report id
        return await Report.deleteOne({ _id: input.id });
      } else if (input.reviewID) {
        //  delete report(s) by review id
        return await Report.deleteMany({ reviewID: input.reviewID });
      } else {
        // no id or reviewID specified
        return false;
      }
    }),
});

export default reportsRouter;
