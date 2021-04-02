import React from 'react'; 

import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route
} from 'react-router-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import AppHeader from './component/AppHeader/AppHeader.jsx';
import Footer from './component/Footer/Footer';

import SearchPage from "./pages/SearchPage";
import CoursePage from "./pages/CoursePage/CoursePage.jsx";
import ErrorPage from "./pages/ErrorPage";
import RoadmapPage from "./pages/RoadmapPage";
import ZotisticsPage from "./pages/ZotisticsPage";
import Schedule from "./component/Schedule/Schedule";
import Sidebar from "./component/SideBar/SideBar";

export default function App() {
  return (
    <Router>
    <AppHeader/>  
        <div style={{display: "flex", padding: "4rem 3rem"}}>
        <Switch>
          <Route exact path="/">
            <Redirect to="/search/courses" />
          </Route>
          <Route path="/search/:index" />
          <Route component={Sidebar} />
        </Switch>
          <Switch>
            <Route exact path="/">
              <Redirect to="/search/courses" />
            </Route>
            <Route path="/roadmap" component={RoadmapPage}/>
            <Route path="/zotistics" component={ZotisticsPage} />
            <Route path="/search/:index" component={SearchPage} />
            <Route path="/course/:id" component={CoursePage} />
            <Route path="/schedule" component = {Schedule}></Route>
            <Route component={ErrorPage} />
          </Switch>
        </div>
      <Footer/>
    </Router>
  )
}
