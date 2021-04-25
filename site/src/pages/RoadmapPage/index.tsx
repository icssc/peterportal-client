import React, { useState, useCallback, useReducer } from "react";
import "./index.scss";
import Planner from "./Planner.jsx";
import SearchSidebar from "./SearchSidebar.jsx";
import { DragDropContext } from "react-beautiful-dnd";
import { data, data1, data2, data3 } from "./dummyData.js";
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
    }
    case "ADD-YEAR": {
      let fall = action.index + "-fall";
      let winter = action.index + "-winter";
      let spring = action.index + "-spring";
      draft["year-plans"][action.index] = {};
      draft["year-plans"][action.index][fall] = [];
      draft["year-plans"][action.index][winter] = [];
      draft["year-plans"][action.index][spring] = [];
    }
    case "REMOVE-YEAR": {
      delete draft["year-plans"][action.year];
    }
  }
});

const RoadmapPage = () => {
  const [yearPlans, setYearPlans] = useState([]);
  const [state, dispatch] = useReducer(dragReducer, {
    "year-plans": {},
    search: data3,
  });

  const handleAddYear = (year) => {
    const newIndex =
      yearPlans.length !== 0 ? yearPlans[yearPlans.length - 1].index + 1 : 1;
    const newYear = {
      index: newIndex,
      startYear: parseInt(year),
      courses: 0,
      units: 0,
    };
    setYearPlans([...yearPlans, newYear]);
    dispatch({
      type: "ADD-YEAR",
      index: newIndex,
    });
  };

  const removeYear = (year) => {
    const filteredPlans = yearPlans.filter((plan) => plan.index !== year);
    setYearPlans(filteredPlans);
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
            yearPlans={yearPlans}
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
