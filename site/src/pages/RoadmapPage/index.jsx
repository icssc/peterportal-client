import React, { useCallback, useReducer, useEffect } from "react";
import "./index.scss";
import Planner from "./Planner.jsx";
import SearchSidebar from "./SearchSidebar.jsx";
import { DragDropContext } from "react-beautiful-dnd";
import { data } from "./dummyData.js";
import produce from "immer";

const dragReducer = produce((draft, action) => {
  switch (action.type) {
    case "MOVE": {
      let toYear = parseInt(action.to.split("-")[0]);
      let fromYear = null;
      let start = null;
      if (action.from !== "search") {
        fromYear = parseInt(action.from.split("-")[0]);
        start = draft["year-plans"][fromYear];
      } else {
        start = draft;
      }

      start[action.from] = start[action.from] || [];
      draft["year-plans"][toYear][action.to] =
        draft["year-plans"][toYear][action.to] || [];
      const [removed] = start[action.from].splice(action.fromIndex, 1);
      draft["year-plans"][toYear][action.to].splice(action.toIndex, 0, removed);
      break;
    }
    case "ADD-YEAR": {
      let fall = action.index + "-fall";
      let winter = action.index + "-winter";
      let spring = action.index + "-spring";
      draft["year-plans"][action.index] = {
        index: action.index,
        startYear: action.startYear,
        courses: action.courses,
        units: action.units,
      };
      draft["year-plans"][action.index][fall] = [];
      draft["year-plans"][action.index][winter] = [];
      draft["year-plans"][action.index][spring] = [];
      break;
    }
    case "REMOVE-YEAR": {
      delete draft["year-plans"][action.year];
      break;
    }
    default:
      return;
  }
});

const RoadmapPage = () => {
  let savedState = JSON.parse(localStorage.getItem("roadmap-state"));
  let defaultState = savedState;
  if (savedState !== null) {
    defaultState = savedState;
  } else {
    defaultState = {
      "year-plans": {},
      search: data,
    };
  }

  const [state, dispatch] = useReducer(dragReducer, defaultState);

  useEffect(() => {
    localStorage.setItem("roadmap-state", JSON.stringify(state));
  }, [state]);

  const handleAddYear = (year) => {
    let yearKeys = Array.from(Object.keys(state["year-plans"]));
    yearKeys.sort();
    const newIndex =
      yearKeys.length !== 0
        ? state["year-plans"][yearKeys[yearKeys.length - 1]].index + 1
        : 1;
    dispatch({
      type: "ADD-YEAR",
      index: newIndex,
      startYear: parseInt(year),
      courses: 0,
      units: 0,
    });
  };

  const removeYear = (year) => {
    dispatch({
      type: "REMOVE-YEAR",
      year: year,
    });
  };

  const onDragEnd = useCallback((result) => {
    if (result.reason === "DROP") {
      if (!result.destination) {
        return;
      }
      if (
        result.destination.droppableId === "search" &&
        result.source.droppableId !== "search"
      ) {
        // Don't move courses back into search area from a quarter
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
    <div className="roadmap-page">
      <DragDropContext onDragEnd={onDragEnd}>
        <div className="main-wrapper">
          <Planner
            handleAddYear={handleAddYear}
            removeYear={removeYear}
            state={state}
          />
        </div>
        <div className="sidebar-wrapper">
          <SearchSidebar state={state} />
        </div>
      </DragDropContext>
    </div>
  );
};

export default RoadmapPage;
