import { useState, FC } from 'react';
import './SearchModule.scss';

import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { SearchIndex } from '../../types/types';
import { setShowSavedCourses } from '../../store/slices/roadmapSlice';
import { setQuery } from '../../store/slices/searchSlice';

import { InputAdornment, IconButton, TextField } from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';
import SearchFilters from '../SearchFilters/SearchFilters.tsx';
import { useSearchTrigger } from '../../hooks/search.ts';

const SEARCH_TIMEOUT_MS = 300;

interface SearchModuleProps {
  index?: SearchIndex;
}

const SearchModule: FC<SearchModuleProps> = () => {
  const index = useAppSelector((state) => state.search.viewIndex);
  const dispatch = useAppDispatch();
  const search = useAppSelector((state) => state.search[index]);
  const [searchQuery, setSearchQuery] = useState('');
  const [pendingRequest, setPendingRequest] = useState<number | null>(null);
  useSearchTrigger();

  const searchImmediately = (query: string) => {
    if (pendingRequest) clearTimeout(pendingRequest);
    if (location.pathname === '/') {
      dispatch(setShowSavedCourses(!query));
    }
    if (query !== search.query) {
      dispatch(setQuery(query));
      setPendingRequest(null);
    }
  };

  const searchAfterTimeout = (query: string) => {
    setSearchQuery(query);
    if (pendingRequest) clearTimeout(pendingRequest);
    const timeout = window.setTimeout(() => searchImmediately(query), SEARCH_TIMEOUT_MS);
    setPendingRequest(timeout);
  };

  const coursePlaceholder = 'Search for a course...';
  const professorPlaceholder = 'Search a professor';
  const placeholder = index === 'courses' ? coursePlaceholder : professorPlaceholder;

  const endAdornment = (
    <InputAdornment position="end">
      <IconButton aria-label="Search" onClick={() => searchImmediately(searchQuery)}>
        <SearchIcon />
      </IconButton>
    </InputAdornment>
  );

  return (
    <div className="search-module">
      <TextField
        variant="outlined"
        className="search-bar"
        aria-label="search"
        type="text"
        placeholder={placeholder}
        onChange={(e) => searchAfterTimeout(e.target.value)}
        defaultValue={search.query}
        autoCorrect="off"
        slotProps={{ input: { endAdornment, className: 'input-wrapper' } }}
      />
      {index === 'courses' && search.query && <SearchFilters />}
    </div>
  );
};

export default SearchModule;
