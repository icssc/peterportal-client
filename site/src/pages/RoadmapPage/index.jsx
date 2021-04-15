import React, { useState, useCallback, useReducer } from "react";
import "./index.scss";
import PlannerPage from "./PlannerPage.jsx";
import SearchSidebar from "./SearchSidebar.jsx";
import { DragDropContext } from "react-beautiful-dnd";
import { data, data1, data2 } from "./dummyData.js";
import produce from "immer";

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

function RoadmapPage() {
  const [yearPlans, setYearPlans] = useState([]);
  const [state, dispatch] = useReducer(dragReducer, {
    "1-fall": data,
    "1-winter": data1,
    "2-spring": data2,
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
  };

  const removeYear = (year) => {
    const filteredPlans = yearPlans.filter((plan) => plan.index !== year);
    setYearPlans(filteredPlans);
  };

  const onDragEnd = useCallback((result) => {
    if (result.reason === "DROP") {
      if (!result.destination) {
        return;
      }
      if (result.destination.droppableId.includes("search")) {
        // Don't move courses back into search area
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
          <PlannerPage
            yearPlans={yearPlans}
            handleAddYear={handleAddYear}
            removeYear={removeYear}
            state={state}
          />
        </div>
        <div className="sidebar-wrapper">
          <SearchSidebar />
        </div>
      </DragDropContext>
    </div>
  );
}

export default RoadmapPage;
