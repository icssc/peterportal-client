import React, { FC } from 'react'
import SearchSidebar from "./SearchSidebar.jsx"
import PlannerModule from "./PlannerModule.jsx"

const RoadmapPage: FC = () => {
  return (
    <div>
      <h1>Peter's Roadmap</h1>
      <SearchSidebar/>
      <PlannerModule />
    </div>
  )
}

export default RoadmapPage
