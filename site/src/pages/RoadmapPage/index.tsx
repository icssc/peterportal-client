import { FC, useCallback } from 'react';
import './index.scss';
import Planner from './Planner';
import SearchSidebar from './SearchSidebar';
import { DragDropContext, DropResult, DragStart } from 'react-beautiful-dnd';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { moveCourse, deleteCourse, setActiveCourse } from '../../store/slices/roadmapSlice';
import AddCoursePopup from './AddCoursePopup';
import { CourseGQLData } from '../../types/types';
import { useIsMobile } from '../../helpers/util';
import RoadmapMultiplan from './RoadmapMultiplan';

const RoadmapPage: FC = () => {
  const dispatch = useAppDispatch();
  const showSearch = useAppSelector((state) => state.roadmap.showSearch);
  const searchResults = useAppSelector((state) => state.search.courses.results) as CourseGQLData[];
  const isMobile = useIsMobile();

  const onDragEnd = useCallback((result: DropResult) => {
    if (result.reason === 'DROP') {
      // no destination
      if (!result.destination) {
        return;
      }

      // dragging to search bar
      if (result.destination.droppableId === 'search') {
        // removing from quarter
        if (result.source.droppableId != 'search') {
          const [yearIndex, quarterIndex] = result.source.droppableId.split('-');
          dispatch(
            deleteCourse({
              yearIndex: parseInt(yearIndex),
              quarterIndex: parseInt(quarterIndex),
              courseIndex: result.source.index,
            }),
          );
        }
        return;
      }

      const movePayload = {
        from: {
          yearIndex: -1,
          quarterIndex: -1,
          courseIndex: -1,
        },
        to: {
          yearIndex: -1,
          quarterIndex: -1,
          courseIndex: -1,
        },
      };

      // roadmap to roadmap has source
      if (result.source.droppableId != 'search') {
        const [yearIndex, quarterIndex] = result.source.droppableId.split('-');
        movePayload.from.yearIndex = parseInt(yearIndex);
        movePayload.from.quarterIndex = parseInt(quarterIndex);
        movePayload.from.courseIndex = result.source.index;
      }
      // search to roadmap has no source (use activeCourse in global state)

      // both have destination
      const [yearIndex, quarterIndex] = result.destination.droppableId.split('-');
      movePayload.to.yearIndex = parseInt(yearIndex);
      movePayload.to.quarterIndex = parseInt(quarterIndex);
      movePayload.to.courseIndex = result.destination.index;

      dispatch(moveCourse(movePayload));
    }
  }, []);

  const onDragStart = useCallback(
    (start: DragStart) => {
      if (start.source.droppableId === 'search') {
        const activeCourse = searchResults[start.source.index];
        dispatch(setActiveCourse(activeCourse));
      }
    },
    [dispatch, searchResults],
  );

  // do not conditionally renderer because it would remount planner which would discard unsaved changes
  const mobileVersion = (
    <>
      <div className={`main-wrapper mobile ${showSearch ? 'hide' : ''}`}>
        <Planner />
      </div>
      <div className={`sidebar-wrapper mobile ${!showSearch ? 'hide' : ''}`}>
        <SearchSidebar />
      </div>
    </>
  );

  const desktopVersion = (
    <>
      <div className="main-wrapper">
        <Planner />
      </div>
      <div className="sidebar-wrapper">
        <SearchSidebar />
      </div>
    </>
  );

  return (
    <>
      <RoadmapMultiplan />
      <div className="roadmap-page">
        <AddCoursePopup />
        <DragDropContext onDragStart={onDragStart} onDragEnd={onDragEnd}>
          {isMobile && mobileVersion}
          {!isMobile && desktopVersion}
        </DragDropContext>
      </div>
    </>
  );
};

export default RoadmapPage;
