import React from 'react';

import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route
} from 'react-router-dom';
import 'bootstrap/dist/css/bootstrap.min.css';
import './App.scss';
import { Collapse, Fade } from 'react-bootstrap';

import AppHeader from './component/AppHeader/AppHeader';
import Footer from './component/Footer/Footer';
import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import ErrorPage from './pages/ErrorPage';
import RoadmapPage from './pages/RoadmapPage';
import ZotisticsPage from './pages/ZotisticsPage';
import SideBar from './component/SideBar/SideBar';

import { useAppSelector } from './store/hooks';

export default function App() {
  const sidebarOpen = useAppSelector(state => state.ui.sidebarOpen);

  return (
    <Router>
      <Collapse in={sidebarOpen} dimension='width'>
        <div>
          <SideBar></SideBar>
        </div>
      </Collapse>
      <AppHeader />
      <div className='app-content'>
        <Switch>
          <Route exact path='/'>
            <Redirect to='/search/courses' />
          </Route>
          <Route path='/search/:index' />
        </Switch>
        <Switch>
          <Route exact path='/'>
            <Redirect to='/search/courses' />
          </Route>
          <Route path='/roadmap' component={RoadmapPage} />
          <Route path='/zotistics' component={ZotisticsPage} />
          <Route path='/search/:index' component={SearchPage} />
          <Route path='/course/:id+' component={CoursePage} />
          <Route path='/professor/:id' component={ProfessorPage} />
          <Route component={ErrorPage} />
        </Switch>
      </div>
      <Footer />
    </Router>
  )
}
