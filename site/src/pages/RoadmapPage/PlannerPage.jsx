import React, { useState } from "react";
import "./PlannerPage.scss";
import Header from "./Header.jsx";

function PlannerPage() {
  const [courseCount, setCourseCount] = useState(0);
  const [unitCount, setUnitCount] = useState(0);

  return (
    <div className="planner-page">
      <Header courseCount={courseCount} unitCount={unitCount} />
      <h1>Planner Page</h1>
    </div>
  );
}

export default PlannerPage;
