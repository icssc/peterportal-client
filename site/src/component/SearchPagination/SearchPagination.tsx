import { FC } from 'react';
import { Pagination } from 'react-bootstrap';
import { NUM_RESULTS_PER_PAGE } from '../../helpers/constants';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setPageNumber } from '../../store/slices/searchSlice';
import { SearchIndex } from '../../types/types';

interface SearchPaginationProps {
  index: SearchIndex;
}

/* SearchPagination is the page buttons at the bottom of the search results */
const SearchPagination: FC<SearchPaginationProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const searchData = useAppSelector((state) => state.search[index]);

  const clickPageNumber = (pageNumber: number) => {
    dispatch(setPageNumber({ index, pageNumber }));
  };

  const numPages = Math.ceil(searchData.count / NUM_RESULTS_PER_PAGE);
  const activePage = searchData.pageNumber;

  // only show 5 page numbers at a time
  const items = [];
  let startPageNumber = Math.max(0, activePage - 2);
  const endPageNumber = Math.min(numPages, startPageNumber + 5); // exclusive
  startPageNumber = Math.max(0, endPageNumber - 5);
  for (let i = startPageNumber; i < endPageNumber; i++) {
    items.push(
      <Pagination.Item key={i} active={i === activePage} onClick={() => clickPageNumber(i)}>
        {i + 1}
      </Pagination.Item>,
    );
  }

  return (
    // hide if there is no page or only one page
    // last button intentionally left out since first 5 pages are fuzzy searched initially (we don't know what the last page # is)
    numPages <= 1 ? null : (
      <Pagination>
        <Pagination.First onClick={() => clickPageNumber(0)} disabled={activePage === 0} />
        <Pagination.Prev onClick={() => clickPageNumber(activePage - 1)} disabled={activePage === 0} />
        {items}
        <Pagination.Next onClick={() => clickPageNumber(activePage + 1)} disabled={activePage === numPages - 1} />
      </Pagination>
    )
  );
};

export default SearchPagination;
