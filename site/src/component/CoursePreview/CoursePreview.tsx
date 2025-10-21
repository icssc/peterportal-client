import './CoursePreview.scss';
import { FC, useEffect, useState } from 'react';
import { ResultPageSection } from '../ResultPageContent/ResultPageContent';
import GradeDist from '../GradeDist/GradeDist';
import PrereqTree from '../PrereqTree/PrereqTree';
import Schedule from '../Schedule/Schedule';
import Review from '../Review/Review';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { checkModalOpen, sortTerms, transformCourseGQL } from '../../helpers/util';
import CourseSummary from './CourseSummary';
import { LOADING_COURSE_PLACEHOLDER } from '../../helpers/courseRequirements';
import trpc from '../../trpc';
import spawnToast from '../../helpers/toastify';
import { CourseGQLData } from '../../types/types';
import { Button, IconButton, Paper, Tooltip } from '@mui/material';
import { CourseBookmarkButton } from '../CourseInfo/CourseInfo';
import { useAppDispatch } from '../../store/hooks';
import { setPreviewedCourse } from '../../store/slices/coursePreviewSlice';
import Twemoji from 'react-twemoji';

import CloseIcon from '@mui/icons-material/Close';
import IosShareIcon from '@mui/icons-material/IosShare';

/** @todo make this a shared hook to read and write to global cache after that's created  */
function useCourseData(courseId: string) {
  const [fullCourseData, setFullCourseData] = useState(LOADING_COURSE_PLACEHOLDER);
  const [loadTrigger, setLoadTrigger] = useState(false);

  useEffect(() => {
    // Use a stateful trigger to avoid sending two requests as a result of double first render
    setLoadTrigger(true);
  }, [courseId]);

  /** @todo read from global cache */
  useEffect(() => {
    if (!loadTrigger) return;
    setLoadTrigger(false);
    setFullCourseData(LOADING_COURSE_PLACEHOLDER);
    trpc.courses.get
      .query({ courseID: courseId })
      .then((course) => {
        setFullCourseData(transformCourseGQL(course));
      })
      .catch(() => {
        spawnToast('There was an error loading this course', true);
      });
  }, [courseId, loadTrigger]);

  return fullCourseData;
}

const CoursePreviewContent: FC<{ data: CourseGQLData }> = ({ data }) => {
  if (data.id === LOADING_COURSE_PLACEHOLDER.id) {
    return <LoadingSpinner />;
  }

  return (
    <div className="preview-body">
      <ResultPageSection title={data.title}>
        <CourseSummary course={data} />
      </ResultPageSection>

      <ResultPageSection title="📊 Grade Distribution">
        <GradeDist course={data} />
      </ResultPageSection>

      <ResultPageSection title="🌲 Prerequisite Tree">
        <PrereqTree key={data.id} {...data} />
      </ResultPageSection>

      <ResultPageSection title="🗓️ Schedule of Classes">
        <Schedule
          key={data.id}
          courseID={data.department + ' ' + data.courseNumber}
          termsOffered={sortTerms(data.terms)}
        />
      </ResultPageSection>

      <ResultPageSection title="💬 Reviews">
        <Review key={data.id} course={data} terms={sortTerms(data.terms)} />
      </ResultPageSection>
    </div>
  );
};

const CoursePreview: FC<{ courseId: string }> = ({ courseId }) => {
  courseId = courseId.replace(/\s/g, '');
  const courseData = useCourseData(courseId);
  const isLoading = courseData.id === LOADING_COURSE_PLACEHOLDER.id;
  const dispatch = useAppDispatch();

  const closePreview = () => dispatch(setPreviewedCourse(''));

  const copyCourseLink = () => {
    const url = new URL('/course/' + courseId, location.origin).toString();
    navigator.clipboard.writeText(url);
    spawnToast('Copied course URL to clipboard!');
  };

  useEffect(() => {
    const listener = (event: KeyboardEvent) => {
      const modified = event.altKey || event.shiftKey || event.ctrlKey || event.metaKey;
      if (event.key !== 'Escape' || modified) return;
      if (checkModalOpen()) return;
      event.preventDefault();
      closePreview();
    };

    document.body.addEventListener('keydown', listener);
    return () => document.body.removeEventListener('keydown', listener);
  });

  return (
    <div className="course-preview">
      <Paper className="preview-header" variant="outlined">
        <Tooltip title="Exit Preview (Esc)">
          <IconButton onClick={closePreview}>
            <CloseIcon />
          </IconButton>
        </Tooltip>
        <p className="preview-title">
          {isLoading ? (
            `Loading ${courseId}...`
          ) : (
            <>
              Previewing{' '}
              <b>
                {courseData.department} {courseData.courseNumber}
              </b>
            </>
          )}
        </p>
        <Button
          variant="contained"
          color="inherit"
          startIcon={<IosShareIcon />}
          size="small"
          disableElevation
          onClick={copyCourseLink}
        >
          Share
        </Button>
        <CourseBookmarkButton course={courseData} disabled={isLoading} />
      </Paper>
      <Twemoji options={{ className: 'twemoji' }}>
        <CoursePreviewContent data={courseData} />
      </Twemoji>
    </div>
  );
};

export default CoursePreview;
