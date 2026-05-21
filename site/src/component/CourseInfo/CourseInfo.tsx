import { FC, useEffect, useState } from 'react';
import { QuarterName } from '@peterportal/types';
import { CourseGQLData } from '../../types/types';
import { quarterDisplayNames } from '../../helpers/planner';
import { useSavedCourses } from '../../hooks/savedCourses';
import { pluralize } from '../../helpers/util';
import './CourseInfo.scss';
import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';

import { Button, IconButton } from '@mui/material';
import BookmarkBorderIcon from '@mui/icons-material/BookmarkBorder';
import BookmarkIcon from '@mui/icons-material/Bookmark';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import { fetchGradeDistData } from '../GradeDist/GradeDist';
import { getAggregateGradeData, GradesAggregate } from '../../helpers/gradeDist.ts';

interface CourseProp {
  course: CourseGQLData;
  disabled?: boolean;
  includeLabel?: boolean;
  clampDescription?: number;
}

export const CourseBookmarkButton: FC<CourseProp> = ({ course, disabled = false, includeLabel = false }) => {
  const { isCourseSaved, toggleSavedCourse } = useSavedCourses();
  const courseIsSaved = isCourseSaved(course);
  if (includeLabel) {
    return (
      <Button
        variant="contained"
        color="inherit"
        startIcon={courseIsSaved ? <BookmarkIcon /> : <BookmarkBorderIcon />}
        size="small"
        disableElevation
        onClick={() => toggleSavedCourse(course)}
      >
        Save
      </Button>
    );
  } else {
    return (
      <IconButton className="bookmark-button" onClick={() => toggleSavedCourse(course)} disabled={disabled}>
        {courseIsSaved ? <BookmarkIcon /> : <BookmarkBorderIcon />}
      </IconButton>
    );
  }
};

export const CourseSynopsis: FC<CourseProp> = ({ course, clampDescription = 0 }) => {
  return (
    <p className="course-info-clamp" style={clampDescription ? { WebkitLineClamp: clampDescription } : {}}>
      <b className="title">{course.title}</b>
      <span className="description">{course.description}</span>
    </p>
  );
};

export const PrerequisiteText: FC<CourseProp> = ({ course, clampDescription = 0 }) => {
  if (!course.prerequisiteText) return <></>;

  return (
    <p className="course-info-clamp" style={clampDescription ? { WebkitLineClamp: clampDescription } : {}}>
      <b>Prerequisites:</b> {course.prerequisiteText}
    </p>
  );
};

export const CorequisiteText: FC<CourseProp> = ({ course }) => {
  if (!course.corequisites) return <></>;

  return (
    <p>
      <b>Corequisites:</b> {course.corequisites}
    </p>
  );
};

export const IncompletePrerequisiteText: FC<{ requiredCourses?: string[] }> = ({ requiredCourses }) => {
  if (!requiredCourses?.length) return;

  return (
    <div className="course-info-warning">
      <div className="warning-primary">
        <WarningAmberIcon className="warning-icon" />
        Prerequisite{pluralize(requiredCourses.length)} Not Met: {requiredCourses.join(', ')}
      </div>
      <div className="warning-hint-italics">
        Already completed? Click the "Credits" tab in the sidebar to add{' '}
        {pluralize(requiredCourses.length, 'these prerequisites', 'this prerequisite')}.
      </div>
      <br />
    </div>
  );
};

export const AverageGPAText: FC<CourseProp> = ({ course }) => {
  const [aggregateGradeData, setAggregateGradeData] = useState<GradesAggregate | null>(null);

  useEffect(() => {
    fetchGradeDistData({ course })
      .then((data) => {
        const aggregateData = getAggregateGradeData(data, 'ALL', 'ALL', 'ALL');
        setAggregateGradeData(aggregateData);
      })
      .catch((error) => {
        setAggregateGradeData(null);
        console.error(error);
      });
  }, [course]);

  let displayAverageGPA = 'Loading...';
  if (aggregateGradeData != null) {
    displayAverageGPA = aggregateGradeData.averageGPA === 'NaN' ? 'N/A' : aggregateGradeData.averageGPA;
  }

  return (
    <p>
      <b>Average GPA:</b> {displayAverageGPA}
    </p>
  );
};

export const PreviousOfferingsRow: FC<CourseProp> = ({ course }) => {
  return (
    <>
      {course.terms && course.terms.length > 0 && (
        <div className="quarter-offerings-section">
          <b>Recent Offerings:</b>
          <RecentOfferingsTable terms={course.terms} size="thin" />
        </div>
      )}
    </>
  );
};

export const TermMismatchText: FC<{ termMismatch?: QuarterName | 'RecentYears' }> = ({ termMismatch }) => {
  if (!termMismatch) return;

  const text =
    termMismatch === 'RecentYears'
      ? 'Not offered in recent years'
      : `Typically not offered in ${quarterDisplayNames[termMismatch]}`;

  return (
    <div className="course-info-warning">
      <br />
      <div className="warning-secondary">
        <WarningAmberIcon className="warning-icon" />
        {text}
      </div>
    </div>
  );
};
