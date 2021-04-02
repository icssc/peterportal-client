import React, { useCallback, useReducer } from "react";
import "./SearchSidebar.scss";
import { InputGroup, FormControl } from "react-bootstrap";
import { Search, PlusCircle } from 'react-bootstrap-icons';
import { DragDropContext, Droppable, Draggable } from 'react-beautiful-dnd';
import produce from "immer";
import { data } from './dummyData.js';

const dragReducer = produce((draft, action) => {
  switch (action.type) {
    case "MOVE": {
      draft[action.from] = draft[action.from] || [];
      draft[action.to] = draft[action.to] || [];
      const [removed] = draft[action.from].splice(action.fromIndex, 1);
      draft[action.to].splice(action.toIndex, 0, removed);
    }
  }
});

function SearchSidebar() {
  const [state, dispatch] = useReducer(dragReducer, { items: data });

  const onDragEnd = useCallback((result) => {
    if (result.reason === "DROP") {
      if (!result.destination) {
        return;
      }
      dispatch({
        type: "MOVE",
        from: result.source.droppableId,
        to: result.destination.droppableId,
        fromIndex: result.source.index,
        toIndex: result.destination.index,
      });
    }
  }, []);

  return (
    <div className="search-sidebar">
      <h1 className="title">Add a course</h1>
      <PlusCircle /> Custom
      <InputGroup className="search">
        <Search />
        <FormControl
          placeholder="Search"
          aria-label="Search"
          className="search-bar"
        />
      </InputGroup>

      <DragDropContext onDragEnd={onDragEnd}>
        <Droppable droppableId="items" type="COURSE">
          {(provided) => {
            return (
              <div
                ref={provided.innerRef}
                {...provided.droppableProps}
                className="quarter"
              >
                {state.items?.map((course, index) => {
                  return (
                    <Draggable
                      key={course.id}
                      draggableId={course.id}
                      index={index}
                    >
                      {(provided) => {
                        return (
                          <div
                            ref={provided.innerRef}
                            {...provided.draggableProps}
                            {...provided.dragHandleProps}
                          >
                            <div className="course">
                              <div className="name">
                                {course.name}
                              </div>
                              <div className="title">
                                {course.title}
                              </div>
                              <div className="units">
                                {course.units} units
                              </div>
                            </div>
                          </div>
                        );
                      }}
                    </Draggable>
                  );
                })}
                {provided.placeholder}
              </div>
            );
          }}
        </Droppable>
      </DragDropContext>
    </div>
  )
}

export default SearchSidebar