import React, { FC, useState, useEffect } from 'react';
import axios, { AxiosResponse } from 'axios';
import SubReview from './SubReview';
import ReviewForm from '../ReviewForm/ReviewForm';
import './Review.scss';

import { selectReviews, setReviews, setFormStatus } from '../../store/slices/reviewSlice';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import { CourseGQLData, ProfessorGQLData, ReviewData, VoteColorsRequest, VoteColor } from '../../types/types';

export interface ReviewProps {
    course?: CourseGQLData;
    professor?: ProfessorGQLData;
}

const Review: FC<ReviewProps> = (props) => {
    const dispatch = useAppDispatch();
    const reviewData = useAppSelector(selectReviews);
    const [voteColors, setVoteColors] = useState([]);
    const openForm = useAppSelector(state => state.review.formOpen);

    const getColors = async (vote: VoteColorsRequest) => {
        const res = await axios.patch('/api/reviews/getVoteColors', vote);
        return res.data;
    }

    const getReviews = async () => {
        interface paramsProps {
            courseID?: string;
            professorID?: string;
        }
        let params: paramsProps = {};
        if (props.course) params.courseID = props.course.id;
        if (props.professor) params.professorID = props.professor.ucinetid;
        axios.get(`/api/reviews`, {
            params: params
        })
            .then(async (res: AxiosResponse<ReviewData[]>) => {
                const data = res.data.filter((review) => review !== null);
                data.sort((a, b) => {
                    let aScore = a.score + (a.verified ? 10000 : 0);
                    let bScore = b.score + (b.verified ? 10000 : 0);
                    return bScore - aScore;
                })
                const req = {
                    ids: data.map(review => review._id!)
                }
                let colors = await getColors(req);
                console.log(colors);
                setVoteColors(colors);
                dispatch(setReviews(data));
            });
    }

    const getVoteColor = (id: string | undefined) => {
        let temp = voteColors as Object;
        let v = (temp[id as keyof typeof temp]) as unknown as number;
        return v ?? 0;
    }

    useEffect(() => {
        // prevent reviews from carrying over
        dispatch(setReviews([]));
        getReviews();
    }, [props.course?.id, props.professor?.ucinetid]);

    const openReviewForm = () => {
        dispatch(setFormStatus(true));
        document.body.style.overflow = 'hidden';
    }
    const closeForm = () => {
        dispatch(setFormStatus(false));
        document.body.style.overflow = 'visible';
    }

    if (!reviewData) {
        return <p>Loading reviews..</p>;
    } else {
        return (
            <>
                <div className='reviews'>
                    {reviewData.map((review, i) => {
                        if (review !== null) return (<SubReview review={review} key={i} course={props.course} professor={props.professor} userVote={getVoteColor(review._id)} />)
                    })}
                    <button type='button' className='add-review-btn' onClick={openReviewForm}>+ Add Review</button>
                </div>
                <ReviewForm closeForm={closeForm} {...props} />
            </>
        )
    }
}

export default Review;