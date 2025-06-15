import { FC, useCallback, useEffect, useRef, useState } from 'react';
import { quarterDisplayNames } from '../../helpers/planner';
import { deepCopy, useIsMobile, pluralize } from '../../helpers/util';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import {
  deleteCourse,
  moveCourse,
  MoveCoursePayload,
  setActiveCourse,
  setShowSearch,
} from '../../store/slices/roadmapSlice';
import { PlannerQuarterData } from '../../types/types';
import './Quarter.scss';

import Course from './Course';
import { ReactSortable, SortableEvent } from 'react-sortablejs';
import { quarterSortable } from '../../helpers/sortable';

import PlaylistAddIcon from '@mui/icons-material/PlaylistAdd';
import { Button, Card } from '@mui/material';

interface QuarterProps {
  yearIndex: number;
  quarterIndex: number;
  data: PlannerQuarterData;
}

const Quarter: FC<QuarterProps> = ({ yearIndex, quarterIndex, data }) => {
  const dispatch = useAppDispatch();
  const quarterTitle = quarterDisplayNames[data.name];
  const invalidCourses = useAppSelector(
    (state) => state.roadmap.plans[state.roadmap.currentPlanIndex].content.invalidCourses,
  );
  const quarterContainerRef = useRef<HTMLDivElement>(null);
  const isMobile = useIsMobile();
  const [moveCourseTrigger, setMoveCourseTrigger] = useState<MoveCoursePayload | null>(null);
  const activeCourseLoading = useAppSelector((state) => state.roadmap.activeCourseLoading);
  const activeCourse = useAppSelector((state) => state.roadmap.activeCourse);
  const unitCount = data.courses.reduce((sum, course) => sum + course.minUnits, 0);
  const coursesCopy = deepCopy(data.courses); // Sortable requires data to be extensible (non read-only)

  const removeCourseAt = useCallback(
    (index: number) => {
      dispatch(deleteCourse({ courseIndex: index, quarterIndex, yearIndex }));
    },
    [dispatch, quarterIndex, yearIndex],
  );

  const removeCourse = (event: SortableEvent) => removeCourseAt(event.oldIndex!);

  const addCourse = async (event: SortableEvent) => {
    const movePayload = {
      from: { yearIndex: -1, quarterIndex: -1, courseIndex: -1 },
      to: { yearIndex, quarterIndex, courseIndex: event.newIndex! },
    };
    if (activeCourseLoading) setMoveCourseTrigger(movePayload);
    else dispatch(moveCourse(movePayload));
  };

  const sortCourse = (event: SortableEvent) => {
    if (event.from !== event.to) return;
    const movePayload = {
      from: { yearIndex, quarterIndex, courseIndex: event.oldDraggableIndex! },
      to: { yearIndex, quarterIndex, courseIndex: event.newDraggableIndex! },
    };
    dispatch(moveCourse(movePayload));
  };

  const setDraggedItem = (event: SortableEvent) => {
    const course = data.courses[event.oldIndex!];
    dispatch(setActiveCourse(course));
  };

  useEffect(() => {
    if (!moveCourseTrigger) return; // nothing to add
    if (activeCourseLoading) {
      dispatch(moveCourse(moveCourseTrigger));
      return; // course to add hasn't loaded yet
    }

    removeCourseAt(moveCourseTrigger.to.courseIndex);
    setMoveCourseTrigger(null);
    dispatch(moveCourse(moveCourseTrigger));
    dispatch(setActiveCourse(undefined));
  }, [dispatch, moveCourseTrigger, activeCourseLoading, removeCourseAt]);

  return (
    <Card className="quarter" ref={quarterContainerRef} variant="outlined">
      <div className="quarter-header">
        <h2 className="quarter-title">{quarterTitle.replace('10 Week', '10wk')}</h2>
        <div className="quarter-units">
          {unitCount} unit{pluralize(unitCount)}
        </div>
        {isMobile && (
          <Button
            startIcon={<PlaylistAddIcon />}
            onClick={() => dispatch(setShowSearch({ show: true, year: yearIndex, quarter: quarterIndex }))}
            size="small"
            variant="contained"
            color="inherit"
            disableElevation
          >
            Add Course
          </Button>
        )}
      </div>
      <ReactSortable
        list={coursesCopy}
        className={`quarter-course-list ${activeCourse ? 'dropzone-active' : ''}`}
        onStart={setDraggedItem}
        onAdd={addCourse}
        onRemove={removeCourse}
        onSort={sortCourse}
        onEnd={() => {
          if (!activeCourseLoading) {
            dispatch(setActiveCourse(undefined));
          }
        }}
        {...quarterSortable}
      >
        {data.courses.map((course, index) => {
          const requiredCourses = invalidCourses.find(
            (ic) =>
              ic.location.courseIndex === index &&
              ic.location.quarterIndex === quarterIndex &&
              ic.location.yearIndex === yearIndex,
          )?.required;

          return (
            <Course
              key={index}
              course={course}
              requiredCourses={requiredCourses}
              onDelete={() => removeCourseAt(index)}
              // addMode="drag" somehow fixes the issue with tapping a course after adding on mobile
              addMode="drag"
            />
          );
        })}
      </ReactSortable>
    </Card>
  );
};

export default Quarter;
