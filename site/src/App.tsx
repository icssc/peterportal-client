import { useCallback, useEffect, useState } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import 'semantic-ui-css/semantic.min.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import './style/theme.scss';
import './App.scss';

import AppHeader from './component/AppHeader/AppHeader';
import ChangelogModal from './component/ChangelogModal/ChangelogModal';
import Footer from './component/Footer/Footer';
import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import ErrorPage from './pages/ErrorPage';
import RoadmapPage from './pages/RoadmapPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';
import SideBar from './component/SideBar/SideBar';

import ThemeContext, { Theme } from './style/theme-context';
import axios from 'axios';
import { useCookies } from 'react-cookie';

function isSystemDark() {
  return window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
}

export default function App() {
  // default darkMode to local or system preferences
  const [usingSystemTheme, setUsingSystemTheme] = useState(
    localStorage.getItem('theme') === 'system' || !localStorage.getItem('theme'),
  );
  const [darkMode, setDarkMode] = useState(
    usingSystemTheme ? isSystemDark() : localStorage.getItem('theme') === 'dark',
  );
  const [cookies] = useCookies(['user']);
  const [prevDarkMode, setPrevDarkMode] = useState(false); // light theme is default on page load

  /**
   * we run this check at render-time and compare with previous state because a useEffect
   * would cause a flicker for dark mode users on page load since the first render would be without
   * the data-theme property set (light would be used by default)
   */
  if (darkMode != prevDarkMode) {
    // Theme styling is controlled by data-theme attribute on body being set to light or dark
    document.body.setAttribute('data-theme', darkMode ? 'dark' : 'light');
    setPrevDarkMode(darkMode);
  }

  /**
   * Sets the theme state
   * @param theme
   */
  const setThemeState = useCallback((theme: Theme) => {
    if (theme === 'system') {
      setDarkMode(isSystemDark());
      setUsingSystemTheme(true);
    } else {
      setDarkMode(theme === 'dark');
      setUsingSystemTheme(false);
    }
  }, []);

  /**
   * Sets the theme state and saves the users theme preference.
   * Saves to account if logged in, local storage if not
   * @param theme
   */
  const setTheme = (theme: Theme) => {
    setThemeState(theme);
    if (cookies.user) {
      axios.post('/api/users/preferences', { theme });
    } else {
      localStorage.setItem('theme', theme);
    }
  };

  useEffect(() => {
    // if logged in, load user prefs (theme) from mongo
    if (cookies.user) {
      axios.get('/api/users/preferences').then((res) => {
        const { theme }: { theme?: Theme } = res.data;
        if (theme) {
          setThemeState(theme);
        }
      });
    }
  }, [cookies.user, setThemeState]);

  return (
    <Router>
      <ThemeContext.Provider value={{ darkMode, usingSystemTheme, setTheme }}>
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
          {/* <div className="changelog-modal">{<ChangelogModal />}</div> */}
        </div>
      </ThemeContext.Provider>
    </Router>
  );
}
