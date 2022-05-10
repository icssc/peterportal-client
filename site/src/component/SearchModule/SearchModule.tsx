import React, { useState, useEffect, Component, FC } from 'react';
import './SearchModule.scss';
import wfs from 'websoc-fuzzy-search';
import axios from 'axios';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import { Search } from 'react-bootstrap-icons';


import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setNames, setResults } from '../../store/slices/searchSlice';

import { SearchIndex, BatchCourseData } from '../../types/types';

const PAGE_SIZE = 10;

interface SearchModuleProps {
    index: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = ({ index }) => {
    const dispatch = useAppDispatch();
    const courseSearch = useAppSelector(state => state.search.courses);
    const professorSearch = useAppSelector(state => state.search.professors);

    // Refresh search results when names and page number changes
    useEffect(() => {
        searchResults('courses', courseSearch.pageNumber, courseSearch.names);
    }, [courseSearch.names, courseSearch.pageNumber])
    useEffect(() => {
        searchResults('professors', professorSearch.pageNumber, professorSearch.names);
    }, [professorSearch.names, professorSearch.pageNumber])

    let searchNames = (query: string) => {
        try {
            /*
                TODO: Search optimization
                - Currently sending a query request for every input change
                - Goal is to have only one query request pending
                - Use setTimeout/clearTimeout to keep track of pending query request
            */
            let nameResults = wfs(query, PAGE_SIZE * 5, index === 'courses' ? ['DEPARTMENT', 'GE_CATEGORY', 'INSTRUCTOR'] : ['DEPARTMENT', 'GE_CATEGORY', 'COURSE']);
            let names = Object.keys(nameResults);
            // TODO: Remove when professors are supported
            if (index == 'professors') {
                names = ['igassko']
            }
            console.log('From frontend search', names);
            dispatch(setNames({ index, names }));
        }
        catch (e) {
            console.log(e)
        }
    }

    let searchResults = async (index: SearchIndex, pageNumber: number, names: string[]) => {
        // Get the subset of names based on the page
        let pageNames = names.slice(PAGE_SIZE * pageNumber, PAGE_SIZE * (pageNumber + 1))
        // Get results from backend search
        axios.post<BatchCourseData>(`/${index}/api/batch`, { [index]: pageNames })
            .then(searchResponse => {
                let results = Object.values(searchResponse.data);
                console.log('From backend search', results);
                dispatch(setResults({ index, results }));
            })
    }

    let coursePlaceholder = 'Search a course number or department';
    let professorPlaceholder = 'Search a professor';
    let placeholder = index == 'courses' ? coursePlaceholder : professorPlaceholder;

    return <div className='search-module'>
        <Form.Group className="mb-3">
            <InputGroup>
                <InputGroup.Prepend>
                    <InputGroup.Text>
                        <Search />
                    </InputGroup.Text>
                </InputGroup.Prepend>
                <Form.Control className='search-bar' type="text" placeholder={placeholder} onChange={(e) => searchNames(e.target.value)} />
            </InputGroup>
        </Form.Group>
    </div>
}

export default SearchModule;