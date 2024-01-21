import { useEffect, useState } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import 'semantic-ui-css/semantic.min.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import './style/theme.scss';
import './App.scss';

import AppHeader from './component/AppHeader/AppHeader';
import Footer from './component/Footer/Footer';
import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import ErrorPage from './pages/ErrorPage';
import RoadmapPage from './pages/RoadmapPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';
import SideBar from './component/SideBar/SideBar';

import ThemeContext from './style/theme-context';
import axios from 'axios';
import { useCookies } from 'react-cookie';

export default function App() {
  // default darkMode to local or system preferences
  const [darkMode, setDarkMode] = useState(
    localStorage.getItem('theme') === 'dark'
      ? true
      : localStorage.getItem('theme') === 'light'
        ? false
        : window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches,
  );
  const [cookies] = useCookies(['user']);

  useEffect(() => {
    // if logged in, load user prefs (theme) from mongo
    if (cookies.user) {
      axios.get('/api/users/preferences').then((res) => {
        const { theme } = res.data;
        if (theme === 'dark') {
          setDarkMode(true);
        } else if (theme === 'light') {
          setDarkMode(false);
        } else {
          // not defined or set to use local/system prefs
          setDarkMode(
            localStorage.getItem('theme') === 'dark'
              ? true
              : localStorage.getItem('theme') === 'light'
                ? false
                : window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches,
          );
          setDarkMode(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
        }
      });
    }
  }, [cookies.user]);

  useEffect(() => {
    document.querySelector('body')!.setAttribute('data-theme', darkMode ? 'dark' : 'light');
    if (cookies.user) {
      axios.post('/api/users/preferences', { theme: darkMode ? 'dark' : 'light', bs: '123', hello: 'world' });
    } else {
      localStorage.setItem('theme', darkMode ? 'dark' : 'light');
    }
  }, [cookies.user, darkMode]);

  const toggleTheme = () => {
    setDarkMode(!darkMode);
  };

  return (
    <Router>
      <ThemeContext.Provider value={{ darkMode: darkMode, toggleTheme: toggleTheme }}>
        <AppHeader />
        <div className="app-body">
          <div className="app-sidebar">
            <SideBar></SideBar>
          </div>
          <div className="app-content">
            <Routes>
              <Route path="/roadmap" element={<RoadmapPage />} />
              <Route path="/" element={<SearchPage />} />
              <Route path="/search/:index" element={<SearchPage />} />
              <Route path="/course/:id" element={<CoursePage />} />
              <Route path="/professor/:id" element={<ProfessorPage />} />
              <Route path="/admin/*" element={<AdminPage />} />
              <Route path="/reviews" element={<ReviewsPage />} />
              <Route path="*" element={<ErrorPage />} />
            </Routes>
            <Footer />
          </div>
        </div>
      </ThemeContext.Provider>
    </Router>
  );
}
