import { api } from '../../index';
import { costToPurchaseAll as costToPurchaseAllQuery } from '../../queries';

interface CostToPurchaseAllFunction {
  (): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

const costToPurchaseAll: CostToPurchaseAllFunction = async () => {
  try {
    const response = await api.post('', {
      query: costToPurchaseAllQuery,
      variables: {},
    });
    return response;
  } catch (error) {
    return null;
  }
};

export default costToPurchaseAll;
