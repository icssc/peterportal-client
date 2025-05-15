import { useEffect, useMemo } from 'react';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import trpc from '../trpc';
import {
  setAllTransferredGEs,
  setAPExams,
  setTransferredCourses,
  setUncategorizedCourses,
  setUserAPExams,
  setUserDataLoaded,
  TransferredCourse,
} from '../store/slices/transferCreditsSlice';
import { components } from '@peterportal/types/src/generated/anteater-api-types';

/** A temporary function that returns the rewarded courses for an AP but always choosing the first choice in any given OR */
type CourseTreeItem = components['schemas']['coursesGrantedTree'] | string;
function naiveCountedCourses(courses: CourseTreeItem): string[] {
  if (typeof courses === 'string') return [courses];
  if ('AND' in courses) return (courses.AND as CourseTreeItem[]).flatMap(naiveCountedCourses);
  return naiveCountedCourses(courses.OR[0]);
}

export function useTransferredCredits() {
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const transferredCourses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const apTransfers = useAppSelector((state) => state.transferCredits.userAPExams);
  const ge = useAppSelector((state) => state.transferCredits.transferredGEs);
  const other = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

  /** @todo add some way to specify the source of a course */
  // i.e. transferType: 'AP' | 'Course'. We could also return two arrays, but it may be more ideal
  // from a dev perspective to have a "single array of truth" rather than opening the door to making
  // mistakes later when counting credits because we chose the wrong courses array

  const courses: TransferredCourse[] = useMemo(() => {
    // Recomputes this value only when APs or Transferred Courses update
    const rewardedCourses = apTransfers.flatMap((transfer) => {
      const info = apExamInfo.find((ap) => ap.fullName === transfer.examName);
      if (!info) return [];
      const reward = info.rewards.find((r) => r.acceptableScores.includes(transfer.score));
      if (!reward) return [];

      const courseNames = naiveCountedCourses(reward.coursesGranted);
      /** @todo debug print statement – remove once a user can choose which 'OR' reward they want */
      // console.log(info.catalogueName ?? info.fullName, '=>', courseNames)

      return courseNames.map((courseName) => ({ courseName, units: 0 }));
    });

    return [...new Set(transferredCourses.concat(rewardedCourses))];
  }, [apExamInfo, apTransfers, transferredCourses]);

  // Returns memoized result for efficiency and so we can use the entire return value
  // as a dependency array item. Without memoization, that would not be possible, as
  // a new object would be created every time.
  const memoized = useMemo(
    () => ({ courses, apInfo: apExamInfo, ap: apTransfers, ge, other }),
    [apExamInfo, courses, apTransfers, ge, other],
  );
  return memoized;
}

export function useLoadTransferredCredits() {
  const userDataLoaded = useAppSelector((state) => state.transferCredits.userDataLoaded);
  const dispatch = useAppDispatch();

  useEffect(() => {
    if (userDataLoaded) return;

    const { getTransferredCourses, getSavedAPExams, getTransferredGEs, getUncategorizedTransfers, getAPExamInfo } =
      trpc.transferCredits;

    const loadAPInfo = getAPExamInfo.query().then((exams) => {
      const parsedExams = exams.map((exam) => ({
        ...exam,
        catalogueName: (exam.catalogueName as string) ?? null,
      }));
      dispatch(setAPExams(parsedExams));
    });

    const loadCourses = getTransferredCourses.query().then((res) => dispatch(setTransferredCourses(res)));
    const loadAPs = getSavedAPExams.query().then((res) => dispatch(setUserAPExams(res)));
    const loadGEs = getTransferredGEs.query().then((res) => dispatch(setAllTransferredGEs(res)));
    const loadOther = getUncategorizedTransfers.query().then((res) => dispatch(setUncategorizedCourses(res)));

    // Load in parallel
    Promise.all([loadAPInfo, loadCourses, loadAPs, loadGEs, loadOther]).then(() => dispatch(setUserDataLoaded(true)));
  }, [dispatch, userDataLoaded]);
}
