import React, { FC, useCallback, useReducer, useEffect } from 'react';
import './index.scss';
import Planner from './Planner';
import SearchSidebar from './SearchSidebar';
import { DragDropContext, DropResult } from 'react-beautiful-dnd';
import { useAppDispatch } from '../../store/hooks';
import { moveCourse } from '../../store/slices/roadmapSlice';
import Error from '../../component/Error/Error';
import { isMobile, isBrowser } from 'react-device-detect';

const RoadmapPage: FC = () => {
  const dispatch = useAppDispatch();

  const onDragEnd = useCallback((result: DropResult) => {
    if (result.reason === 'DROP') {
      // no destination
      if (!result.destination) {
        return;
      }

      // roadmap to search / search to search
      if (result.destination.droppableId === 'search') {
        // Don't move courses back into search area
        return;
      }

      let movePayload = {
        from: {
          yearIndex: -1,
          quarterIndex: -1,
          courseIndex: -1,
        },
        to: {
          yearIndex: -1,
          quarterIndex: -1,
          courseIndex: -1
        }
      };

      console.log(result.source.droppableId, '=>', result.destination.droppableId)

      // roadmap to roadmap has source
      if (result.source.droppableId != 'search') {
        let [yearIndex, quarterIndex] = result.source.droppableId.split('-');
        movePayload.from.yearIndex = parseInt(yearIndex);
        movePayload.from.quarterIndex = parseInt(quarterIndex);
        movePayload.from.courseIndex = result.source.index;
      }
      // search to roadmap has no source (use activeCourse in global state)      

      // both have destination
      let [yearIndex, quarterIndex] = result.destination.droppableId.split('-');
      movePayload.to.yearIndex = parseInt(yearIndex);
      movePayload.to.quarterIndex = parseInt(quarterIndex);
      movePayload.to.courseIndex = result.destination.index;

      console.log(movePayload);
      dispatch(moveCourse(movePayload));
    }
  }, []);

  return (
    <>
      {isMobile &&
        <Error message='This page is under construction for mobile!' />
      }
      {isBrowser &&
        <div className='roadmap-page'>
          <DragDropContext onDragEnd={onDragEnd} onDragStart={(result) => { }}>
            <div className='main-wrapper' id='screenshot'>
              <Planner />
            </div>
            <div className='sidebar-wrapper'>
              <SearchSidebar />
            </div>
          </DragDropContext>
        </div>
      }
    </>
  );
};

export default RoadmapPage;
