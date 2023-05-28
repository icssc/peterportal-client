import React, { useEffect, useState } from 'react';

import {
  BrowserRouter as Router,
  Switch, Redirect,
  Route
} from 'react-router-dom';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import './style/theme.scss';
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
import SideBar from './component/SideBar/SideBar';

import { useAppSelector } from './store/hooks';
import { darkTheme, lightTheme } from './style/theme';
import { ThemeProvider } from 'styled-components';
import ThemeContext from './style/theme-context';

export default function App() {
  const [darkMode, setDarkMode] = useState(localStorage.getItem('darkMode') === 'true' ? true : (localStorage.getItem('darkMode') === 'false' ? false : (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches)));
  const sidebarOpen = useAppSelector(state => state.ui.sidebarOpen);
  const [isShown, setIsShown] = useState(false);

  useEffect(() => {
    localStorage.setItem('darkMode', darkMode ? 'true' : 'false');
    document.querySelector('body')!.setAttribute('data-theme', darkMode ? 'dark' : 'light');
  }, [darkMode])

  const toggleTheme = () => {
    setDarkMode(!darkMode);
  }

  return (
    <Router basename={process.env.PUBLIC_URL}>
      <ThemeContext.Provider value={{ darkMode: darkMode, toggleTheme: toggleTheme }} >
        <AppHeader/>
        <div className='app-body'>
          <div className='app-sidebar'>
            <SideBar/>
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
              <Route component={ErrorPage} />
            </Switch>
            <Footer />
          </div>
        </div>
      </ThemeContext.Provider>
    </Router>
  )
}