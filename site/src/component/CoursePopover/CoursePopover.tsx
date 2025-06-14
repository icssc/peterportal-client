import { FC } from 'react';
import './CoursePopover.scss';
import Popover from 'react-bootstrap/Popover';
import { CourseGQLData } from '../../types/types';
import { CourseHeader, AllCourseInfo } from '../CourseInfo/CourseInfo';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';

interface CoursePopoverProps {
  course: CourseGQLData | string;
}

const CoursePopover: FC<CoursePopoverProps> = ({ course }) => (
  <Popover.Content className="course-popover">
    {typeof course === 'string' ? (
      <LoadingSpinner />
    ) : (
      <>
        <div className="popover-name">
          <CourseHeader course={course} />
        </div>
        <AllCourseInfo course={course} />
      </>
    )}
  </Popover.Content>
);

export default CoursePopover;
