import React, { useState, useCallback, useReducer } from "react";
import "./index.scss";
import PlannerPage from "./PlannerPage.jsx";
import SearchSidebar from "./SearchSidebar.jsx";
import { DragDropContext } from "react-beautiful-dnd";
import { data, data1, data2 } from "./dummyData.js";

const dragReducer = (state, action) => {
  return state;
};

function RoadmapPage() {
  const [yearPlans, setYearPlans] = useState([]);
  const [state, dispatch] = useReducer(dragReducer, {
    items: data,
    items1: data1,
    items2: data2,
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

  const onDragEnd = useCallback((result) => {}, []);

  return (
    <div className="roadmap-page">
      <DragDropContext onDragEnd={onDragEnd}>
        <div className="main-wrapper">
          <PlannerPage
            yearPlans={yearPlans}
            handleAddYear={handleAddYear}
            removeYear={removeYear}
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
