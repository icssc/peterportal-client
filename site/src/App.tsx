import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import 'semantic-ui-css/semantic.min.css';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'react-bootstrap-range-slider/dist/react-bootstrap-range-slider.css';
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

export default function App() {
  return (
    <Router>
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
    </Router>
  );
}
