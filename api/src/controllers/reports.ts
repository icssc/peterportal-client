/**
 @module ReportsRoute
*/

import Report, { ReportType } from '../models/report';
import { adminProcedure, publicProcedure, router } from '../helpers/trpc';
import typia from 'typia';

const reportsRouter = router({
  get: adminProcedure.query(async () => {
    const reports = await Report.find();
    return reports;
  }),
  add: publicProcedure.input(typia.createAssert<ReportType>()).mutation(async ({ input }) => {
    const report = new Report(input);
    await report.save();

    return input;
  }),
  delete: adminProcedure.input(typia.createAssert<{ id?: string; reviewID?: string }>()).mutation(async ({ input }) => {
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
