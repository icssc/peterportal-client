import { FC } from 'react';
import { Pagination } from '@mui/material';
import './SearchPagination.scss';

import { setPageNumber } from '../../store/slices/searchSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';

/* SearchPagination is the page buttons at the bottom of the search results */
const SearchPagination: FC = () => {
  const dispatch = useAppDispatch();
  const viewIndex = useAppSelector((state) => state.search.viewIndex);
  const searchData = useAppSelector((state) => state.search[viewIndex]);

  /* MUI Pagination is 1-indexed, so we adjust for this */
  const clickPageNumber = (_: React.ChangeEvent<unknown>, pageNumber: number) => {
    dispatch(setPageNumber(pageNumber - 1));
  };

  const numPages = Math.ceil(searchData.count / NUM_RESULTS_PER_PAGE);
  const activePage = searchData.pageNumber + 1;

  // hide if there is no page or only one page
  if (numPages <= 1) return null;

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
