import { useDispatch, useSelector } from 'react-redux';
import SetCompletionStatusSelector from '../../../../components/search-form-components/SetCompletionStatusSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetCompletionStatusSelectorProps extends ConnectedSearchFormComponentProps {
  setCompletionStatuses: any;
}

const ConnectedSetCompletionStatusSelector: React.FC<ConnectedSetCompletionStatusSelectorProps> = ({
  reduxSlice,
  setCompletionStatuses,
}) => {
  const dispatch = useDispatch();
  const { setCompletionStatuses: completionStatuses } = useSelector((state: RootState) => state[reduxSlice]);

  const updateCompletionStatuses = (completionStatusesValue) => {
    dispatch(setCompletionStatuses({ setCompletionStatuses: completionStatusesValue }));
  };

  return <SetCompletionStatusSelector completionStatuses={completionStatuses} setCompletionStatuses={updateCompletionStatuses} />;
};

export default ConnectedSetCompletionStatusSelector;
