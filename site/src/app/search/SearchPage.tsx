'use client';
import { FC, useEffect } from 'react';
import './SearchPage.scss';
import 'react-multi-carousel/lib/styles.css';
import CoursePopup from './CoursePopup';
import ProfessorPopup from './ProfessorPopup';
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';

import { SearchIndex } from '../../types/types';

const SearchPage: FC<{ index: SearchIndex }> = ({ index }) => {
  useEffect(() => {
    document.title = `${index === 'courses' ? 'Courses' : 'Professors'} | PeterPortal`;
  }, [index]);

  return (
    <>
      <div className="search-wrapper">
        <div className="search-list">
          <SearchModule index={index} />
          <SearchHitContainer />
        </div>
        {index == 'courses' && <CoursePopup />}
        {index == 'professors' && <ProfessorPopup />}
      </div>
    </>
  );
};

export default SearchPage;
