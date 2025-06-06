import { useCallback, useEffect } from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';

import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import 'toastify-js/src/toastify.css';
import './style/theme.scss';
import './App.scss';

import trpc from './trpc';

import AppHeader from './component/AppHeader/AppHeader';
import AppThemeProvider from './component/AppThemeProvider/AppThemeProvider';
import ChangelogModal from './component/ChangelogModal/ChangelogModal';
import SideBar from './component/SideBar/SideBar';

import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import ErrorPage from './pages/ErrorPage';
import RoadmapPage from './pages/RoadmapPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';

import { setCoursebag } from './store/slices/coursebagSlice';
import { useAppDispatch } from './store/hooks';
import { useIsLoggedIn } from './hooks/isLoggedIn';
import { sortCoursebag } from './helpers/coursebag';
import { searchAPIResults } from './helpers/util';

export default function App() {
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();

  const loadCoursebag = useCallback(async () => {
    const courseIds = isLoggedIn
      ? await trpc.savedCourses.get.query()
      : JSON.parse(localStorage.getItem('coursebag') ?? '[]');
    const coursebagData = await searchAPIResults('courses', courseIds);
    const coursebag = sortCoursebag(Object.values(coursebagData));
    dispatch(setCoursebag(coursebag));
  }, [dispatch, isLoggedIn]);

  useEffect(() => {
    loadCoursebag();
  }, [loadCoursebag]);

  return (
    <Router>
      <AppThemeProvider>
        <AppHeader />
        <div className="app-body">
          <SideBar />
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
          </div>
          <ChangelogModal />
        </div>
      </AppThemeProvider>
    </Router>
  );
}
