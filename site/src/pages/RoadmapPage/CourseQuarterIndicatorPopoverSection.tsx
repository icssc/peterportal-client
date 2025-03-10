import { FC } from 'react';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import { CourseGQLData } from '../../types/types';

interface CourseQuarterIndicatorPopoverSectionProps {
  course: CourseGQLData;
}

const CourseQuarterIndicatorPopoverSection: FC<CourseQuarterIndicatorPopoverSectionProps> = ({ course }) => {
  return (
    <>
      {course.terms && course.terms.length > 0 && (
        <p className="quarter-offerings-section">
          <b>Previous Offerings:</b>
          <CourseQuarterIndicator terms={course.terms} size="sm" />
        </p>
      )}
    </>
  );
};

export default CourseQuarterIndicatorPopoverSection;
