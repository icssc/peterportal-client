import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';

import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import 'toastify-js/src/toastify.css';
import './style/theme.scss';
import './App.scss';

import AppHeader from './component/AppHeader/AppHeader';
import AppThemeProvider from './component/AppThemeProvider/AppThemeProvider';
import ChangelogModal from './component/ChangelogModal/ChangelogModal';
import SideBar from './component/SideBar/SideBar';

import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import RoadmapPage from './pages/RoadmapPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';
import ErrorPage from './pages/ErrorPage';

import { useLoadSavedCourses } from './hooks/savedCourses';

export default function App() {
  useLoadSavedCourses();

  return (
    <Router>
      <AppThemeProvider>
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
          </div>
          <div className="changelog-modal">{<ChangelogModal />}</div>
        </div>
      </AppThemeProvider>
    </Router>
  );
}
