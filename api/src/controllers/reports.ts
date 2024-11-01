/**
 @module ReportsRoute
*/

import { adminProcedure, publicProcedure, router } from '../helpers/trpc';
import { z } from 'zod';
import { ReportData, reportSubmission } from '@peterportal/types';
import { db } from '../db';
import { report } from '../db/schema';
import { eq } from 'drizzle-orm';

const reportsRouter = router({
  /**
   * Get all reports
   */
  get: adminProcedure.query(async () => {
    return (await db.select().from(report)).map((report) => ({
      ...report,
      createdAt: report.createdAt.toISOString(),
    })) as ReportData[];
  }),
  /**
   * Add a report
   */
  add: publicProcedure.input(reportSubmission).mutation(async ({ input }) => {
    await db.insert(report).values(input);
    return input;
  }),
  /**
   * Delete a report
   */
  delete: adminProcedure
    .input(z.object({ id: z.number().optional(), reviewId: z.number().optional() }))
    .mutation(async ({ input }) => {
      if (input.id) {
        // delete report by report id
        await db.delete(report).where(eq(report.id, input.id));
        return true;
      } else if (input.reviewId) {
        //  delete report(s) by review id
        await db.delete(report).where(eq(report.reviewId, input.reviewId));
        return true;
      } else {
        // no id or reviewID specified
        return false;
      }
    }),
});

export default reportsRouter;
