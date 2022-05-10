import React, { FC, useState } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import './SearchPage.scss';
import 'react-multi-carousel/lib/styles.css';
import { Fade } from 'react-bootstrap';
import CoursePopup from './CoursePopup'
import ProfessorPopup from './ProfessorPopup'
import SearchModule from '../../component/SearchModule/SearchModule';
import SearchHitContainer from '../../component/SearchHitContainer/SearchHitContainer';

import { useAppDispatch, useAppSelector } from '../../store/hooks';

import { SearchIndex } from '../../types/types';

interface SearchPageProps extends RouteComponentProps<{ index: SearchIndex }> {
}

const SearchPage: FC<SearchPageProps> = (props) => {
    let index = props.match.params.index;
    return <>
        <div id='content-container'>
            <div id='search-list'>
                <SearchModule index={index} />
                <SearchHitContainer index={index} />
            </div>
            <div id="search-popup">
                {index == 'courses' && <CoursePopup />}
                {index == 'professors' && <ProfessorPopup />}
            </div>
        </div>
    </>
}

export default SearchPage;