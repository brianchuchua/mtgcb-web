import { useEffect } from 'react';
import { useDispatch } from 'react-redux';

export const useFormVisibility = (setFormVisibilityAction) => {
  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibilityAction({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibilityAction({ isFormVisibile: false }));
    };
  }, [dispatch, setFormVisibilityAction]);
};
