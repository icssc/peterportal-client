import { FC, useEffect, useState } from "react";
import UserReviews from '../../component/UserReviews/UserReviews';
import Error from '../../component/Error/Error';
import './ReviewsPage.scss';
import { useLocation } from "react-router-dom";
import { useCookies } from "react-cookie";

const ReviewsPage: FC = () => {
    const location = useLocation();
    const [loaded, setLoaded] = useState<boolean>(false);
    const [cookies] = useCookies(['user']);
    const [authorized, setAuthorized] = useState<boolean>(false);

    // user has to be logged in to view this page
    const checkLoggedIn = async () => {
        const loggedIn = cookies.user !== undefined;
        setAuthorized(loggedIn);
        setLoaded(true);
    }

    useEffect(() => {
        checkLoggedIn();
    }, []);

    if (!loaded) {
        return <p>Loading...</p>;
    }
    else if (!authorized) {
        return (
            <Error message='Access Denied: Log in to view this page.'></Error>
        );
    }
    else {
        if (location.pathname.includes('reviews')) {
            return <UserReviews />
        }
    }
    return <Error message='Invalid Reviews Page'></Error>
};

export default ReviewsPage;