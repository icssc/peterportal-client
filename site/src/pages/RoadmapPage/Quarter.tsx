import { FC, useContext, useRef, useState } from 'react';
import { Button, OverlayTrigger, Popover } from 'react-bootstrap';
import { Plus, ThreeDots } from 'react-bootstrap-icons';
import { quarterDisplayNames } from '../../helpers/planner';
import { useIsMobile } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { clearQuarter, deleteCourse, deleteQuarter, setShowSearch } from '../../store/slices/roadmapSlice';
import ThemeContext from '../../style/theme-context';
import { PlannerQuarterData } from '../../types/types';
import './Quarter.scss';

import Course from './Course';
import { SortableContext, verticalListSortingStrategy } from '@dnd-kit/sortable';

interface QuarterProps {
  year: number;
  yearIndex: number;
  quarterIndex: number;
  data: PlannerQuarterData;
}

const Quarter: FC<QuarterProps> = ({ year, yearIndex, quarterIndex, data }) => {
  const dispatch = useAppDispatch();
  const quarterTitle = quarterDisplayNames[data.name];
  const [invalidCourses, activeCourse, overContainer] = useAppSelector((state) => [
    state.roadmap.plans[state.roadmap.currentPlanIndex].content.invalidCourses,
    state.roadmap.activeCourse,
    state.roadmap.overContainer,
  ]);
  // const activeCourse = useAppSelector((state) =>)
  const quarterContainerRef = useRef<HTMLDivElement>(null);
  const isMobile = useIsMobile();
  const [showQuarterMenu, setShowQuarterMenu] = useState(false);

  const { darkMode } = useContext(ThemeContext);
  const buttonVariant = darkMode ? 'dark' : 'light';
  const containerId = `${yearIndex}-${quarterIndex}`;

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

  const removeCourseAt = (index: number) => {
    dispatch(deleteCourse({ courseIndex: index, quarterIndex, yearIndex }));
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

  const courseAddedToOtherContainer = (index: number) => {
    const sortableId = `${containerId}-${index}`;
    return overContainer !== undefined && overContainer !== containerId && sortableId === activeCourse?.id;
  };

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
      <SortableContext
        id={`${containerId}`}
        items={[
          ...data.courses
            .map((_, index) => `${containerId}-${index}`)
            .filter((_, index) => !courseAddedToOtherContainer(index)),
          ...(overContainer === containerId ? [activeCourse!.id] : []),
        ]}
        // items={data.courses.map((course, index) => course.id + index)}
        strategy={verticalListSortingStrategy}
        // onStart={setDraggedItem}
        // onAdd={addCourse}
        // onRemove={removeCourse}
        // onSort={sortCourse}
        // {...quarterSortable}
      >
        <div className="quarter-course-list">
          {data.courses.map((course, index) => {
            const sortableId = `${containerId}-${index}`;
            if (courseAddedToOtherContainer(index)) {
              return undefined;
            }
            let requiredCourses: string[] = null!;
            // if this is an invalid course, set the required courses
            invalidCourses.forEach((ic) => {
              const loc = ic.location;
              if (loc.courseIndex == index && loc.quarterIndex == quarterIndex && loc.yearIndex == yearIndex) {
                requiredCourses = ic.required;
              }
            });

            return (
              <Course
                key={course.id + index}
                sortableId={sortableId}
                {...course}
                requiredCourses={requiredCourses}
                onDelete={() => removeCourseAt(index)}
              />
            );
          })}
          {overContainer === containerId && activeCourse && (
            <Course sortableId={activeCourse.id} {...activeCourse.course} />
          )}
        </div>
      </SortableContext>

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
