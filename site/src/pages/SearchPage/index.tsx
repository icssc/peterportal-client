import { FC } from 'react';
import { useParams } from 'react-router-dom';
import './SearchPage.scss';
import 'react-multi-carousel/lib/styles.css';
import CoursePopup from './CoursePopup';
import ProfessorPopup from './ProfessorPopup';
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';
import CourseHitItem from './CourseHitItem';
import ProfessorHitItem from './ProfessorHitItem';

import { SearchIndex } from '../../types/types';

const SearchPage: FC = () => {
  const { index = 'courses' } = useParams<{ index: SearchIndex }>();
  return (
    <>
      <div id="content-container">
        <div id="search-list">
          <SearchModule index={index} />
          <SearchHitContainer index={index} CourseHitItem={CourseHitItem} ProfessorHitItem={ProfessorHitItem} />
        </div>
        <div id="search-popup">
          {index == 'courses' && <CoursePopup />}
          {index == 'professors' && <ProfessorPopup />}
        </div>
      </div>
    </>
  );
};

export default SearchPage;
