import { useEffect, useState } from 'react';
import { LOADING_COURSE_PLACEHOLDER } from '../helpers/courseRequirements';
import { transformCourseGQL } from '../helpers/util';
import trpc from '../trpc';
import { useAppDispatch, useAppSelector } from '../store/hooks';
import { setToastMsg, setToastSeverity, setShowToast } from '../store/slices/roadmapSlice';
import { setCourse, selectCourseCatalog } from '../store/slices/courseCatalogSlice';

export function useCourseData(courseId: string) {
  const [fullCourseData, setFullCourseData] = useState(LOADING_COURSE_PLACEHOLDER);
  const [loadTrigger, setLoadTrigger] = useState(false);

  const dispatch = useAppDispatch();
  const courseCache = useAppSelector(selectCourseCatalog);

  useEffect(() => {
    // Use a stateful trigger to avoid sending two requests as a result of double first render
    setLoadTrigger(true);
  }, [courseId]);

  useEffect(() => {
    if (!loadTrigger) return;
    setLoadTrigger(false);

    const cachedCourse = courseCache[courseId];

    if (cachedCourse) {
      setFullCourseData(cachedCourse);
      return;
    }

    setFullCourseData(LOADING_COURSE_PLACEHOLDER);
    trpc.courses.get
      .query({ courseID: courseId })
      .then((course) => {
        const transformedCourse = transformCourseGQL(course);
        setFullCourseData(transformedCourse);
        dispatch(setCourse({ courseId, data: transformedCourse }));
      })
      .catch(() => {
        dispatch(setToastMsg('Copied course URL to clipboard!'));
        dispatch(setToastSeverity('success'));
        dispatch(setShowToast(true));
      });
  }, [courseId, dispatch, loadTrigger, courseCache]);

  return fullCourseData;
}
