import React, { FC } from 'react';
import { RouteComponentProps } from 'react-router-dom';
import './SearchPage.scss';
import 'react-multi-carousel/lib/styles.css';
import SearchModule from './SearchModule'
import SearchFilter from './SearchFilter'
import SearchHitContainer from './SearchHitContainer'
import { SearchkitComponent, SearchkitManager, SearchkitProvider, SearchkitComponentProps } from 'searchkit';
import { Collapse } from 'react-bootstrap';
import CoursePopup from '../../component/CoursePopup/CoursePopup'
import ProfessorPopup from '../../component/ProfessorPopup/ProfessorPopup'

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setFilterStatus } from '../../store/slices/uiSlice';

import { ElasticSearchIndex } from '../../types/types';

interface SearchPageProps extends RouteComponentProps<{ index: ElasticSearchIndex }>, SearchkitComponentProps {
}

class SearchPage extends SearchkitComponent<SearchPageProps, {}> {
    // const filterOpen = useAppSelector(state => state.ui.filterOpen);

    render() {
        // 'this.props.match.params.index' is used to determine which index to 
        // query via url location - i.e: (professor || courses)
        let index = this.props.match.params.index;
        let searchkit = new SearchkitManager('/' + index);

        return (
            <SearchkitProvider searchkit={searchkit}>
                <SearchPageContent index={index} />
            </SearchkitProvider>
        );
    }
}

interface SearchPageContentProps {
    index: ElasticSearchIndex;
}
const SearchPageContent: FC<SearchPageContentProps> = ({ index }) => {
    const filterOpen = useAppSelector(state => state.ui.filterOpen);

    return <>
        <div style={{ display: 'flex', flexGrow: 1 }}>
            <div style={{ width: '50vw' }}>
                <Collapse in={filterOpen}>
                    <div>
                        <SearchFilter query={index} />
                    </div>
                </Collapse>
                <SearchModule query={index} />
                <SearchHitContainer query={index} />
            </div>
            <div style={{ flexGrow: 1 }}>
                {index == 'courses' && <CoursePopup />}
                {index == 'professors' && <ProfessorPopup />}
            </div>
        </div>
    </>
}

export default SearchPage;