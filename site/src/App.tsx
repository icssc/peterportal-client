import React, { useState } from 'react';

import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route
} from 'react-router-dom';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import './App.scss';
import { Fade } from 'react-bootstrap';

import AppHeader from './component/AppHeader/AppHeader';
import Footer from './component/Footer/Footer';
import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import ErrorPage from './pages/ErrorPage';
import RoadmapPage from './pages/RoadmapPage';
import ZotisticsPage from './pages/ZotisticsPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';
import SideBar from './component/SideBar/SideBar';

import { useAppSelector } from './store/hooks';

export default function App() {
  const sidebarOpen = useAppSelector(state => state.ui.sidebarOpen);
  const [isShown, setIsShown] = useState(false);

  return (
    <Router basename={process.env.PUBLIC_URL}>
      <AppHeader />
      <div className='app-body'>
        <div className='app-sidebar'>
          <SideBar></SideBar>
        </div>
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
            <Route path='/admin' component={AdminPage} />
            <Route path='/reviews'  component={ReviewsPage} />
            <Route component={ErrorPage} />
          </Switch>
          <Footer />
        </div>
      </div>
    </Router>
  )
}