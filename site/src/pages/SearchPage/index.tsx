import { FC, useEffect } from 'react';
import { useParams } from 'react-router-dom';
import './SearchPage.scss';
import 'react-multi-carousel/lib/styles.css';

import CoursePopup from './CoursePopup';
import ProfessorPopup from './ProfessorPopup';
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import { SearchIndex } from '../../types/types';

const SearchPage: FC = () => {
  const { index = 'courses' } = useParams<{ index: SearchIndex }>();

  useEffect(() => {
    document.title = `${index === 'courses' ? 'Courses' : 'Professors'} | PeterPortal`;
  }, [index]);

  return (
    <div className="search-wrapper">
      <div className="search-list">
        <SearchModule index={index} />
        <SearchHitContainer index={index} />
      </div>
      {index === 'courses' ? <CoursePopup /> : <ProfessorPopup />}
    </div>
  );
};

export default SearchPage;
