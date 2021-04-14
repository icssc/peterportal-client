import React, { useState } from "react";
import "./index.scss";
import PlannerPage from "./PlannerPage.jsx";
import SearchSidebar from "./SearchSidebar.jsx";

function RoadmapPage() {
  const [yearPlans, setYearPlans] = useState([]);

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

  return (
    <div className="roadmap-page">
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
    </div>
  );
}

export default RoadmapPage;
