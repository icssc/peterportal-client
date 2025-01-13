import { FC } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { InfoCircle, ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import CoursePopover from '../../component/CoursePopover/CoursePopover';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';
import { useIsMobile } from '../../helpers/util';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';

interface CourseProps extends CourseGQLData {
  requiredCourses?: string[];
  unmatchedPrerequisites?: string[];
  onDelete?: () => void;
  onAddToBag?: () => void;
  isInBag?: boolean;
  removeFromBag?: () => void;
  openPopoverLeft?: boolean;
}

const Course: FC<CourseProps> = (props) => {
  const {
    id,
    department,
    courseNumber,
    title,
    minUnits,
    maxUnits,
    description,
    prerequisiteText,
    corequisites,
    requiredCourses,
    terms,
    onDelete,
    onAddToBag,
    isInBag,
    removeFromBag,
    openPopoverLeft,
  } = props;

  const courseRoute = '/course/' + props.department.replace(/\s+/g, '') + props.courseNumber.replace(/\s+/g, '');
  const isMobile = useIsMobile();

  return (
    <div className={`course ${requiredCourses ? 'invalid' : ''}`}>
      <div className="course-card-top">
        <div className="course-and-info">
          <span>
            <a className="name" href={courseRoute} target="_blank" rel="noopener noreferrer">
              {department + ' ' + courseNumber}
            </a>
            <span className="units">, {minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units</span>
          </span>
          <OverlayTrigger
            trigger={['hover', 'focus']}
            placement={isMobile ? 'bottom' : openPopoverLeft ? 'left-start' : 'right-start'}
            overlay=<Popover className="ppc-popover" id={'course-popover-' + id}>
              <CoursePopover
                department={department}
                courseNumber={courseNumber}
                title={title}
                minUnits={minUnits}
                maxUnits={maxUnits}
                description={description}
                prerequisiteText={prerequisiteText}
                corequisites={corequisites}
                requiredCourses={requiredCourses}
              />
            </Popover>
            delay={100}
          >
            {requiredCourses ? <ExclamationTriangle /> : <InfoCircle />}
          </OverlayTrigger>
        </div>
        {onDelete ? (
          <ThemeContext.Consumer>
            {({ darkMode }) => (
              <Button
                variant={darkMode ? 'dark' : 'light'}
                className="course-delete-btn"
                onClick={onDelete}
                aria-label="delete"
              >
                <Trash className="course-delete-icon" />
              </Button>
            )}
          </ThemeContext.Consumer>
        ) : (
          <CourseQuarterIndicator terms={terms} size="xs" />
        )}
      </div>
      <div className="title">{title}</div>
      <div className="course-footer">
        {onAddToBag && !isInBag && <BagPlus onClick={onAddToBag}></BagPlus>}
        {isInBag && <BagFill onClick={removeFromBag}></BagFill>}
        {/* <div className="course-footer">
        {requiredCourses && (
          <OverlayTrigger trigger={['hover', 'focus']} placement="right" overlay={WarningPopover} delay={100}>
            <ExclamationTriangle />
          </OverlayTrigger>
        )}
        {/* <div className="units">{minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units</div> * /}
      </div> */}
      </div>
    </div>
  );
};

export default Course;
