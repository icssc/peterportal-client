import { FC } from 'react';
import './Course.scss';
import { Button } from 'react-bootstrap';
import { InfoCircle, ExclamationTriangle, Trash, BagPlus, BagFill } from 'react-bootstrap-icons';
import CourseQuarterIndicator from '../../component/QuarterTooltip/CourseQuarterIndicator';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Popover from 'react-bootstrap/Popover';

import { CourseGQLData } from '../../types/types';
import ThemeContext from '../../style/theme-context';

interface CourseProps extends CourseGQLData {
  requiredCourses?: string[];
  unmatchedPrerequisites?: string[];
  onDelete?: () => void;
  onAddToBag?: () => void;
  isInBag?: boolean;
  removeFromBag?: () => void;
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
  } = props;
  const CoursePopover = (
    <Popover className="ppc-popover" id={'course-popover-' + id}>
      <Popover.Content className="course-popover">
        <div className="popover-name">
          {department + ' ' + courseNumber + ' '}
          <span className="popover-units">({minUnits === maxUnits ? minUnits : `${minUnits}-${maxUnits}`} units)</span>
        </div>
        <div className="popover-description">
          <span className="popover-subtitle">{title + ':'}</span>
          <br />
          {description}
        </div>
        {prerequisiteText && (
          <div className="popover-detail">
            <span className="popover-detail-prefix">Prerequisites:</span> {prerequisiteText}
          </div>
        )}
        {corequisites && (
          <div className="popover-detail">
            <span className="popover-detail-prefix">Corequisites:</span> {corequisites}
          </div>
        )}
        {requiredCourses && (
          <div className="popover-detail">
            <div className="popover-detail-warning">
              <ExclamationTriangle className="popover-detail-warning-icon" />
              Prerequisite{requiredCourses?.length > 1 ? 's' : ''} Not Met: {requiredCourses?.join(', ')}
            </div>
            <div className="popover-detail-italics">
              Already completed? Click "Transfer Credits" at the top of the roadmap viewer to add{' '}
              {requiredCourses?.length > 1 ? 'these prerequisites' : 'this prerequisite'}.
            </div>
          </div>
        )}
      </Popover.Content>
    </Popover>
  );

  const courseRoute = '/course/' + props.department.replace(/\s+/g, '') + props.courseNumber.replace(/\s+/g, '');

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
          <OverlayTrigger trigger={['hover', 'focus']} placement="right" overlay={CoursePopover} delay={100}>
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
