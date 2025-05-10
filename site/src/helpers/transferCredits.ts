import { UncategorizedCourseEntry } from '../pages/RoadmapPage/transfers/UncategorizedCreditsSection';
import { TransferredCourse, UserAPExam } from '../store/slices/transferCreditsSlice';
import { APExam, TransferredGE } from '@peterportal/types';

/**
 * Gets a list of names of courses and AP Exams from transferred credits. Courses and AP Exams
 * are the only things that are used to clear prerequisites and corequisites (and thus validate the planner)
 * @param courses The list of transferred courses that the user has
 * @param apExams The list of AP Exams the user has added credits for
 * @returns ...
 */
export function getNamesOfTransfers(
  courses: TransferredCourse[],
  apExams: UserAPExam[],
  apExamInfo: APExam[],
): string[] {
  // a prerequisite examName may be "AP COMP SCI A", "AP CALCULUS BC" (catalogue name?????)
  const transferNames: string[] = [];

  apExams.forEach((exam) => {
    const examInfo = apExamInfo.find((info) => info.fullName === exam.examName);
    transferNames.push((examInfo?.catalogueName as string | null) ?? exam.examName);
  });

  courses.forEach((course) => {
    course.courseName; // courseName currently doesn't have spaces, will need to fix that
    transferNames.push(course.courseName);
  });

  return transferNames;
}

/**
 * Returns the total number of units across transferred credits
 * @param courses The list of transferred courses that the user has
 * @param apExams The list of AP Exams the user has added credits for
 * @param otherTransfers A list of other (uncategorized) transferred units
 */
export function getTotalUnitsFromTransfers(
  courses: TransferredCourse[],
  apExams: UserAPExam[],
  geTransfers: TransferredGE[],
  otherTransfers: UncategorizedCourseEntry[],
) {
  let total = 0;

  courses.forEach((course) => {
    total += course.units;
  });
  apExams.forEach((apExam) => {
    total += apExam.units;
  });
  geTransfers.forEach((ge) => {
    total += ge.units;
  });
  otherTransfers.forEach((otherTransfer) => {
    total += otherTransfer.units ?? 0;
  });

  return total;
}
