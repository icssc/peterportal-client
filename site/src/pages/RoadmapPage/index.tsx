import React from 'react'
import "./index.scss";
import PlannerPage from "./PlannerPage.jsx"
import SearchSidebar from "./SearchSidebar.jsx"
// import { Grid } from 'semantic-ui-react'

function RoadmapPage() {
  return (
    <div className="roadmap-page">
      <div className="main-wrapper">
        <PlannerPage />
      </div>
      <div className="sidebar-wrapper">
        <SearchSidebar />
      </div>
      {/* <Grid columns={2}>
        <Grid.Row>
          <Grid.Column style={{ width: "65vw" }}>
            <PlannerPage />
          </Grid.Column>
          <Grid.Column style={{ width: "15vw" }}>
            <SearchSidebar />
          </Grid.Column>
        </Grid.Row>
      </Grid> */}
    </div>
  )
}

export default RoadmapPage
