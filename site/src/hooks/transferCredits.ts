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
} from '../store/slices/transferCreditsSlice';

export function useTransferredCredits() {
  const apExamInfo = useAppSelector((state) => state.transferCredits.apExamInfo);
  const courses = useAppSelector((state) => state.transferCredits.transferredCourses);
  const apTransfers = useAppSelector((state) => state.transferCredits.userAPExams);
  const ge = useAppSelector((state) => state.transferCredits.transferredGEs);
  const other = useAppSelector((state) => state.transferCredits.uncategorizedCourses);

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
