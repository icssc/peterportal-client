import React from 'react'
import "./index.scss";
import PlannerPage from "./PlannerPage.jsx"
import SearchSidebar from "./SearchSidebar.jsx"

function RoadmapPage() {
  return (
    <div className="roadmap-page">
      <div className="main-wrapper">
        <PlannerPage />
      </div>
      <div className="sidebar-wrapper">
        <SearchSidebar />
      </div>
    </div>
  )
}

export default RoadmapPage
