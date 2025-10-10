import './CoursePreview.scss';
import { FC } from 'react';
import { ResultPageSection } from '../ResultPageContent/ResultPageContent';
import GradeDist from '../GradeDist/GradeDist';
import PrereqTree from '../PrereqTree/PrereqTree';
import Schedule from '../Schedule/Schedule';
import Review from '../Review/Review';
import { CourseGQLData } from '../../types/types';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { sortTerms } from '../../helpers/util';
import CourseSummary from './CourseSummary';

export interface CoursePreviewProps {
  data: CourseGQLData | 'loading';
}

const CoursePreview: FC<CoursePreviewProps> = (props) => {
  if (props.data === 'loading') return <LoadingSpinner />;

  const course = props.data;

  return (
    <div className="course-preview">
      <ResultPageSection title={course.title}>
        <CourseSummary course={course} />
      </ResultPageSection>

      <ResultPageSection title="📊 Grade Distribution">
        <GradeDist course={course} />
      </ResultPageSection>

      <ResultPageSection title="🌲 Prerequisite Tree">
        <PrereqTree key={course.id} {...course} />
      </ResultPageSection>

      <ResultPageSection title="🗓️ Schedule of Classes">
        <Schedule
          key={course.id}
          courseID={course.department + ' ' + course.courseNumber}
          termsOffered={sortTerms(course.terms)}
        />
      </ResultPageSection>

      <ResultPageSection title="💬 Reviews">
        <Review key={course.id} course={course} terms={sortTerms(course.terms)} />
      </ResultPageSection>
    </div>
  );
};

export default CoursePreview;
