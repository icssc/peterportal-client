import { FC, useContext, useRef, useState } from 'react';
import { Draggable } from 'react-beautiful-dnd';
import { Button, OverlayTrigger, Popover } from 'react-bootstrap';
import { Plus, ThreeDots } from 'react-bootstrap-icons';
import { quarterDisplayNames } from '../../helpers/planner';
import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { clearQuarter, deleteCourse, deleteQuarter, setShowSearch } from '../../store/slices/roadmapSlice';
import ThemeContext from '../../style/theme-context';
import { PlannerQuarterData } from '../../types/types';
import './Quarter.scss';
import { StrictModeDroppable } from './StrictModeDroppable';

import Course from './Course';

interface QuarterProps {
  year: number;
  yearIndex: number;
  quarterIndex: number;
  data: PlannerQuarterData;
}

const Quarter: FC<QuarterProps> = ({ year, yearIndex, quarterIndex, data }) => {
  const dispatch = useAppDispatch();
  const quarterTitle = quarterDisplayNames[data.name];
  const invalidCourses = useAppSelector(
    (state) => state.roadmap.plans[state.roadmap.currentPlanIndex].content.invalidCourses,
  );
  const quarterContainerRef = useRef<HTMLDivElement>(null);
  const isMobile = useIsMobile();
  const [showQuarterMenu, setShowQuarterMenu] = useState(false);

  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';

  const handleQuarterMenuClick = () => {
    setShowQuarterMenu(!showQuarterMenu);
  };

  const calculateQuarterStats = () => {
    let unitCount = 0;
    let courseCount = 0;
    data.courses.forEach((course) => {
      unitCount += course.minUnits;
      courseCount += 1;
    });
    return [unitCount, courseCount];
  };

  const unitCount = calculateQuarterStats()[0];

  const renderCourses = () => {
    return data.courses.map((course, index) => {
      return (
        <Draggable
          key={`quarter-course-${index}`}
          draggableId={`${yearIndex}-${quarterIndex}-${course.id}-${index}`}
          index={index}
        >
          {(provided) => {
            let requiredCourses: string[] = null!;
            // if this is an invalid course, set the required courses
            invalidCourses.forEach((ic) => {
              const loc = ic.location;
              if (loc.courseIndex == index && loc.quarterIndex == quarterIndex && loc.yearIndex == yearIndex) {
                requiredCourses = ic.required;
              }
            });

            const onDelete = () => {
              dispatch(
                deleteCourse({
                  yearIndex,
                  quarterIndex,
                  courseIndex: index,
                }),
              );
            };

            return (
              <div
                ref={provided.innerRef}
                {...provided.draggableProps}
                {...provided.dragHandleProps}
                className="quarter-course"
                style={{
                  cursor: 'grab',
                  ...provided.draggableProps.style,
                }}
              >
                <Course key={course.id} {...course} requiredCourses={requiredCourses} onDelete={onDelete} />
              </div>
            );
          }}
        </Draggable>
      );
    });
  };

  const popover = (
    <Popover id={`quarter-menu-${yearIndex}-${quarterIndex}`} className="quarter-menu-popover">
      <Popover.Content>
        <div>
          <Button
            variant={buttonVariant}
            className="quarter-menu-btn red-menu-btn"
            onClick={() => {
              dispatch(clearQuarter({ yearIndex: yearIndex, quarterIndex: quarterIndex }));
              setShowQuarterMenu(false);
            }}
          >
            Clear
          </Button>
          <Button
            variant={buttonVariant}
            className="quarter-menu-btn red-menu-btn"
            onClick={() => {
              dispatch(deleteQuarter({ yearIndex: yearIndex, quarterIndex: quarterIndex }));
              setShowQuarterMenu(false);
            }}
          >
            Delete
          </Button>
        </div>
      </Popover.Content>
    </Popover>
  );

  return (
    <div className="quarter" ref={quarterContainerRef}>
      <div className="quarter-header">
        <h2 className="quarter-title">
          {quarterTitle} {year}
        </h2>
        <div className="quarter-units">
          {unitCount} {unitCount === 1 ? 'unit' : 'units'}
        </div>
        <OverlayTrigger
          trigger="click"
          overlay={popover}
          rootClose
          onToggle={setShowQuarterMenu}
          show={showQuarterMenu}
          container={quarterContainerRef}
          placement="bottom"
        >
          {({ ref, ...triggerHandler }) => (
            <button ref={ref} {...triggerHandler} onClick={handleQuarterMenuClick} className="quarter-edit-btn">
              <ThreeDots />
            </button>
          )}
        </OverlayTrigger>
      </div>
      <StrictModeDroppable droppableId={yearIndex + '-' + quarterIndex} type="COURSE">
        {(provided) => {
          return (
            <div ref={provided.innerRef} {...provided.droppableProps} className="quarter-course-list">
              {renderCourses()}
              {provided.placeholder}
            </div>
          );
        }}
      </StrictModeDroppable>

      {isMobile && (
        <>
          <Button
            variant={buttonVariant}
            className="quarter-add-course"
            onClick={() => {
              dispatch(setShowSearch({ show: true, year: yearIndex, quarter: quarterIndex }));
            }}
          >
            <Plus className="plus-icon" />
            <span>Add Course</span>
          </Button>
        </>
      )}
    </div>
  );
};

export default Quarter;
