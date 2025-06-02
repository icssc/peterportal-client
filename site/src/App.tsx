import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';

import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
import './style/theme.scss';
import './App.scss';
import 'toastify-js/src/toastify.css';

import AppThemeProvider from './component/AppThemeProvider/AppThemeProvider';
import AppHeader from './component/AppHeader/AppHeader';
import SideBar from './component/SideBar/SideBar';
import ChangelogModal from './component/ChangelogModal/ChangelogModal';

import RoadmapPage from './pages/RoadmapPage';
import SearchPage from './pages/SearchPage';
import CoursePage from './pages/CoursePage';
import ProfessorPage from './pages/ProfessorPage';
import AdminPage from './pages/AdminPage';
import ReviewsPage from './pages/ReviewsPage';
import ErrorPage from './pages/ErrorPage';

import { useLoadCoursebag } from './hooks/coursebag';

export default function App() {
  useLoadCoursebag();

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
