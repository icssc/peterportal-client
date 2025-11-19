import { FC } from 'react';
import { Pagination } from '@mui/material';
import './SearchPagination.scss';

import { SearchIndex } from '../../types/types';
import { setPageNumber } from '../../store/slices/searchSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';

interface SearchPaginationProps {
  index: SearchIndex;
}

/* SearchPagination is the page buttons at the bottom of the search results */
const SearchPagination: FC<SearchPaginationProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const searchData = useAppSelector((state) => state.search[index]);

  /* MUI Pagination is 1-indexed, so we adjust for this */
  const clickPageNumber = (_: React.ChangeEvent<unknown>, pageNumber: number) => {
    dispatch(setPageNumber({ index, pageNumber: pageNumber - 1 }));
  };

  const numPages = Math.ceil(searchData.count / NUM_RESULTS_PER_PAGE);
  const activePage = searchData.pageNumber + 1;

  return (
    <div className="search-pagination">
      <Pagination
        count={numPages}
        showFirstButton
        page={activePage}
        siblingCount={2}
        onChange={clickPageNumber}
        variant="outlined"
        shape="rounded"
      />
    </div>
  );
};

export default SearchPagination;
