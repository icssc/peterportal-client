import { FC, useEffect } from "react";
import { Pagination } from "react-bootstrap";
import { PAGE_SIZE } from "src/helpers/constants";
import { assert } from "src/helpers/util";
import { useAppDispatch, useAppSelector } from "src/store/hooks";
import { setPageNumber } from "src/store/slices/searchSlice";
import { SearchIndex } from "src/types/types";

interface SearchPaginationProps {
  index: SearchIndex;
}

const SearchPagination: FC<SearchPaginationProps> = ({ index }) => {
  const dispatch = useAppDispatch();
  const coursePageNumber = useAppSelector(state => state.search.courses.pageNumber);
  const professorPageNumber = useAppSelector(state => state.search.professors.pageNumber);

  const courseData = useAppSelector(state => state.search.courses);
  const professorData = useAppSelector(state => state.search.professors);

  const clickPageNumber = (pageNumber: number) => {
    dispatch(setPageNumber({index, pageNumber}));
  }

  // assert in case SerachIndex type is changed in the future
  assert(index === 'courses' || index === 'professors', 'Pagination only supports courses and professors')

  const numPages = index === 'courses' ? 
    Math.ceil(courseData.names.length / PAGE_SIZE) : 
    Math.ceil(professorData.names.length / PAGE_SIZE);
  const active = index === 'courses' ? courseData.pageNumber : courseData.pageNumber;
  let items = [];
  let startPageNumber = Math.max(0, active - 2);
  let endPageNumber = Math.min(numPages, startPageNumber + 5); // exclusive
  startPageNumber = Math.max(0, endPageNumber - 5);
  for (let i = startPageNumber; i < endPageNumber; i++) {
    items.push(
      <Pagination.Item key={i} active={i === active} onClick={() => clickPageNumber(i)}>
        {i + 1}
      </Pagination.Item>
    );
  }

  return (
    // hide if there is no page or only one page
    numPages <= 1 ? null :
    <Pagination>
      <Pagination.First onClick={() => clickPageNumber(0)} disabled={active === 0} />
      <Pagination.Prev onClick={() => clickPageNumber(active - 1)} disabled={active === 0} />
      {items}
      <Pagination.Next onClick={() => clickPageNumber(active + 1)} disabled={active === numPages - 1} />
      <Pagination.Last onClick={() => clickPageNumber(numPages - 1)} disabled={active === numPages - 1} />
    </Pagination>
  );
}

export default SearchPagination;